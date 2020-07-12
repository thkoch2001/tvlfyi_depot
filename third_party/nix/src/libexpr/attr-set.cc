#include "libexpr/attr-set.hh"

#include <new>

#include <absl/container/btree_map.h>
#include <gc/gc_cpp.h>
#include <glog/logging.h>

#include "libexpr/eval-inline.hh"
#include "libutil/visitor.hh"

namespace nix {

BindingsIterator& BindingsIterator::operator++() {
  std::visit(overloaded{
                 [](AttributeMap::iterator& iter) { ++iter; },
                 [](AttributeVector::iterator& iter) { ++iter; },
             },
             _iterator);
  return *this;
}

BindingsIterator BindingsIterator::operator++(int) {
  std::visit(overloaded{
                 [](AttributeMap::iterator& iter) { iter++; },
                 [](AttributeVector::iterator& iter) { iter++; },
             },
             _iterator);
  return *this;
}

bool BindingsIterator::operator==(const BindingsIterator& other) const {
  return _iterator == other._iterator;
}

bool BindingsIterator::operator!=(const BindingsIterator& other) const {
  return _iterator != other._iterator;
}

BindingsIterator::reference BindingsIterator::operator*() const {
  return std::visit(
      overloaded{
          [](AttributeMap::iterator iter) -> std::pair<const Symbol, Attr>& {
            return *iter;
          },
          [](AttributeVector::iterator iter) -> std::pair<const Symbol, Attr>& {
            // this cast is effectively upcasting the left-hand side of the
            // pair, which in the vector case *must* be const so that insert and
            // friends can shift it around, but in the map case *must not* be
            // const so that the key ordering semantics don't change out from
            // under the map. So we pick const as the LUB of the two types and
            // then upcast here. The static_assert, per the docs for
            // reinterpret_cast, is proving that this is safe
            static_assert(
                std::is_standard_layout<std::pair<const Symbol, Attr>>::value);
            return *reinterpret_cast<std::pair<const Symbol, Attr>*>(&*iter);
          },
      },
      _iterator);
}

class BTreeBindings : public Bindings {
 public:
  size_t size() override;
  bool empty() override;
  void push_back(const Attr& attr) override;
  void insert_or_assign(Attr& attr) override;
  Bindings::iterator find(const Symbol& name) override;
  Bindings::iterator begin() override;
  Bindings::iterator end() override;
  void merge(Bindings& other) override;
  [[deprecated]] virtual std::vector<const Attr*> lexicographicOrder() override;

 private:
  AttributeMap attributes_;
};

// This function inherits its name from previous implementations, in
// which Bindings was backed by an array of elements which was scanned
// linearly.
//
// In that setup, inserting duplicate elements would always yield the
// first element (until the next sort, which wasn't stable, after
// which things are more or less undefined).
//
// This behaviour is mimicked by using .insert(), which will *not*
// override existing values.
void BTreeBindings::push_back(const Attr& attr) {
  auto [_, inserted] = attributes_.insert({attr.name, attr});

  if (!inserted) {
    DLOG(WARNING) << "attempted to insert duplicate attribute for key '"
                  << attr.name << "'";
  }
}

// Insert or assign (i.e. replace) a value in the attribute set.
void BTreeBindings::insert_or_assign(Attr& attr) {
  attributes_.insert_or_assign(attr.name, attr);
}

size_t BTreeBindings::size() { return attributes_.size(); }

bool BTreeBindings::empty() { return attributes_.empty(); }

std::vector<const Attr*> BTreeBindings::lexicographicOrder() {
  std::vector<const Attr*> res;
  res.reserve(attributes_.size());

  for (const auto& [key, value] : attributes_) {
    res.emplace_back(&value);
  }

  return res;
}

Bindings::iterator BTreeBindings::find(const Symbol& name) {
  return BindingsIterator{attributes_.find(name)};
}

Bindings::iterator BTreeBindings::begin() {
  return BindingsIterator{attributes_.begin()};
}

Bindings::iterator BTreeBindings::end() {
  return BindingsIterator{attributes_.end()};
}

void BTreeBindings::merge(Bindings& other) {
  for (auto& [key, value] : other) {
    this->attributes_[key] = value;
  }
}

void EvalState::mkAttrs(Value& v, size_t capacity) {
  clearValue(v);
  v.type = tAttrs;
  v.attrs = Bindings::NewGC();
  assert(v.attrs->begin() == v.attrs->begin());
  assert(v.attrs->end() == v.attrs->end());
  nrAttrsets++;
  nrAttrsInAttrsets += capacity;
}

/* Create a new attribute named 'name' on an existing attribute set stored
   in 'vAttrs' and return the newly allocated Value which is associated with
   this attribute. */
Value* EvalState::allocAttr(Value& vAttrs, const Symbol& name) {
  Value* v = allocValue();
  vAttrs.attrs->push_back(Attr(name, v));
  return v;
}

class VectorBindings : public Bindings {
 public:
  size_t size() override;
  bool empty() override;
  void push_back(const Attr& attr) override;
  void insert_or_assign(Attr& attr) override;
  Bindings::iterator find(const Symbol& name) override;
  Bindings::iterator begin() override;
  Bindings::iterator end() override;
  void merge(Bindings& other) override;
  [[deprecated]] virtual std::vector<const Attr*> lexicographicOrder() override;

 private:
  AttributeVector attributes_;
};

size_t VectorBindings::size() { return attributes_.size(); }

bool VectorBindings::empty() { return attributes_.empty(); }

// Insert or assign (i.e. replace) a value in the attribute set.
void VectorBindings::insert_or_assign(Attr& attr) {
  for (auto it = attributes_.begin(); it != attributes_.end(); ++it) {
    if (it->first == attr.name) {
      it->second = attr;
      return;
    } else if (attr.name < it->first) {
      // TODO convert to BTreeMap if we get big enough
      attributes_.emplace(it, attr.name, attr);
      return;
    }
  }

  attributes_.emplace_back(attr.name, attr);
}

void VectorBindings::merge(Bindings& other) {
  AttributeVector new_attributes(size() + other.size());

  auto m_it = attributes_.begin();
  auto other_it = other.begin();

  while (other_it != other.end() && m_it != attributes_.end()) {
    if (other_it->first < m_it->first) {
      new_attributes.push_back(*(m_it++));
    } else {
      if (m_it->first == other_it->first) {
        ++m_it;
      }
      new_attributes.push_back(*(other_it++));
    }
  }

  if (m_it != attributes_.end()) {
    std::copy(m_it, attributes_.end(), std::back_inserter(new_attributes));
  }

  if (other_it != other.end()) {
    std::copy(other_it, other.end(), std::back_inserter(new_attributes));
  }

  new_attributes.shrink_to_fit();

  attributes_ = new_attributes;
}

void VectorBindings::push_back(const Attr& attr) {
  attributes_.emplace_back(attr.name, attr);
}

std::vector<const Attr*> VectorBindings::lexicographicOrder() {
  std::vector<const Attr*> result(attributes_.size());

  for (auto& [_, attr] : attributes_) {
    result.push_back(&attr);
  }

  return result;
}

Bindings::iterator VectorBindings::find(const Symbol& name) {
  return BindingsIterator{
      std::find_if(attributes_.begin(), attributes_.end(),
                   [&name](const auto& pair) { return pair.first == name; })};
}

Bindings::iterator VectorBindings::begin() {
  return BindingsIterator{attributes_.begin()};
}

Bindings::iterator VectorBindings::end() {
  return BindingsIterator{attributes_.end()};
}

// TODO pick what to do based on size
Bindings* Bindings::NewGC() { return new (GC) BTreeBindings; }
// Bindings* Bindings::NewGC() { return new (GC) VectorBindings; }

}  // namespace nix

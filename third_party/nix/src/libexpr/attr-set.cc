#include "libexpr/attr-set.hh"

#include <new>

#include <absl/container/btree_map.h>
#include <gc/gc_cpp.h>
#include <glog/logging.h>

#include "libexpr/eval-inline.hh"

namespace nix {

BindingsIterator& BindingsIterator::operator++() {
  _iterator++;
  return *this;
}
BindingsIterator BindingsIterator::operator++(int) {
  ++_iterator;
  return *this;
}
bool BindingsIterator::operator==(const BindingsIterator& other) const {
  return _iterator == other._iterator;
}
bool BindingsIterator::operator!=(const BindingsIterator& other) const {
  return _iterator != other._iterator;
}
BindingsIterator::reference BindingsIterator::operator*() const {
  return *_iterator;
}

class BTreeBindings : public Bindings {
 public:
  size_t size() override;
  bool empty() override;
  void push_back(const Attr& attr) override;
  void insert_or_assign(const Attr& attr) override;
  Bindings::iterator find(const Symbol& name) override;
  Bindings::iterator begin() override;
  Bindings::iterator end() override;
  void merge(Bindings& other) override;
  [[deprecated]] virtual std::vector<const Attr*> lexicographicOrder() override;

 private:
  AttributeMap attributes_;
};

Bindings* Bindings::NewGC() { return new (GC) BTreeBindings; }

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
void BTreeBindings::insert_or_assign(const Attr& attr) {
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
  v.attrs = BTreeBindings::NewGC();
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

}  // namespace nix

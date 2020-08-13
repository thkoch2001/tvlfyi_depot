#include "libexpr/attr-set.hh"

#include <new>

#include <absl/container/btree_map.h>
#include <glog/logging.h>

#include "libexpr/eval-inline.hh"

namespace nix {

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
void Bindings::push_back(const Attr& attr) {
  auto [_, inserted] = attributes_.insert({attr.name, attr});

  if (!inserted) {
    DLOG(WARNING) << "attempted to insert duplicate attribute for key '"
                  << attr.name << "'";
  }
}

size_t Bindings::size() const { return attributes_.size(); }

bool Bindings::empty() { return attributes_.empty(); }

std::vector<const Attr*> Bindings::SortedByKeys() {
  std::vector<const Attr*> res;
  res.reserve(attributes_.size());

  for (const auto& [key, value] : attributes_) {
    res.emplace_back(&value);
  }

  return res;
}

Bindings::iterator Bindings::find(const Symbol& name) {
  return attributes_.find(name);
}

bool Bindings::Equal(const Bindings* other, EvalState& state) const {
  if (this == other) {
    return true;
  }

  if (this->attributes_.size() != other->attributes_.size()) {
    return false;
  }

  Bindings::const_iterator i;
  Bindings::const_iterator j;
  for (i = this->cbegin(), j = other->cbegin(); i != this->cend(); ++i, ++j) {
    if (i->second.name != j->second.name ||
        !state.eqValues(*i->second.value, *j->second.value)) {
      return false;
    }
  }

  return true;
}

Bindings::iterator Bindings::begin() { return attributes_.begin(); }
Bindings::iterator Bindings::end() { return attributes_.end(); }
Bindings::const_iterator Bindings::cbegin() const {
  return attributes_.cbegin();
}

Bindings::const_iterator Bindings::cend() const { return attributes_.cend(); }

std::unique_ptr<Bindings> Bindings::New(size_t capacity) {
  if (capacity == 0) {
    // TODO(tazjin): A lot of 0-capacity Bindings are allocated.
    // It would be nice to optimize that.
  }

  return std::make_unique<Bindings>();
}

std::unique_ptr<Bindings> Bindings::Merge(const Bindings& lhs,
                                          const Bindings& rhs) {
  auto bindings = New(lhs.size() + rhs.size());

  // Values are merged by inserting the entire iterator range of both
  // input sets. The right-hand set (the values of which take
  // precedence) is inserted *first* because the range insertion
  // method does not override values.
  bindings->attributes_.insert(rhs.attributes_.cbegin(),
                               rhs.attributes_.cend());
  bindings->attributes_.insert(lhs.attributes_.cbegin(),
                               lhs.attributes_.cend());

  return bindings;
}

void EvalState::mkAttrs(Value& v, size_t capacity) {
  clearValue(v);
  v.type = tAttrs;
  v.attrs = Bindings::New(capacity);
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

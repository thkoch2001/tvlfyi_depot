#include "attr-set.hh"

#include <absl/container/btree_map.h>

#include "eval-inline.hh"

namespace nix {

// TODO: using insert_or_assign might break existing Nix code because
// of the weird ordering situation. Need to investigate.
void Bindings::push_back(const Attr& attr) {
  attributes_.insert_or_assign(attr.name, attr);
}

size_t Bindings::size() { return attributes_.size(); }

void Bindings::sort() {}
size_t Bindings::capacity() { return 0; }

bool Bindings::empty() { return attributes_.empty(); }

std::vector<const Attr*> Bindings::lexicographicOrder() {
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

Bindings::iterator Bindings::begin() {
  return attributes_.begin();
}

Bindings::iterator Bindings::end() {
  return attributes_.end();
}

void Bindings::merge(Bindings* other) {
  // We want the values from the other attribute set to take
  // precedence, but .merge() works the other way around.
  //
  // To work around that, we merge and then swap.
  other->attributes_.merge(attributes_);
  attributes_.swap(other->attributes_);
}

// /* Allocate a new array of attributes for an attribute set with a specific
//    capacity. The space is implicitly reserved after the Bindings structure.
//    */
Bindings* EvalState::allocBindings(size_t _capacity) { return new Bindings; }

// TODO(tazjin): What's Value? What's going on here?
void EvalState::mkAttrs(Value& v, size_t capacity) {
  if (capacity == 0) {
    v = vEmptySet;
    return;
  }
  clearValue(v);
  v.type = tAttrs;
  v.attrs = new Bindings;
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

// void Bindings::sort() { std::sort(begin(), end()); }

}  // namespace nix

#include "attr-set.hh"

#include <new>

#include <absl/container/btree_map.h>
#include <gc/gc_cpp.h>

#include "eval-inline.hh"

namespace nix {

// TODO: using insert_or_assign might break existing Nix code because
// of the weird ordering situation. Need to investigate.
void Bindings::push_back(const Attr& attr) {
  attributes_.insert_or_assign(attr.name, attr);
}

size_t Bindings::size() { return attributes_.size(); }

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

Bindings::iterator Bindings::begin() { return attributes_.begin(); }

Bindings::iterator Bindings::end() { return attributes_.end(); }

void Bindings::merge(Bindings* other) {
  // We want the values from the other attribute set to take
  // precedence, but .merge() works the other way around.
  //
  // To work around that, we merge and then swap.
  other->attributes_.merge(attributes_);
  attributes_.swap(other->attributes_);
}

Bindings* Bindings::NewGC() { return new (GC) Bindings; }

void EvalState::mkAttrs(Value& v, size_t capacity) {
  if (capacity == 0) {
    v = vEmptySet;
    return;
  }
  clearValue(v);
  v.type = tAttrs;
  v.attrs = Bindings::NewGC();
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

#include "libexpr/attr-set.hh"

#include <new>

#include <absl/container/btree_map.h>
#include <gc/gc_cpp.h>
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

// Insert or assign (i.e. replace) a value in the attribute set.
void Bindings::insert_or_assign(const Attr& attr) {
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

void Bindings::merge(const Bindings& other) {
  for (auto& [key, value] : other.attributes_) {
    this->attributes_[key] = value;
  }
}

Bindings* Bindings::NewGC() { return new (GC) Bindings; }

void EvalState::mkAttrs(Value& v, size_t capacity) {
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

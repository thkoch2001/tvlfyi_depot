// This file implements the underlying structure of Nix attribute sets.
#pragma once

#include <absl/container/btree_map.h>

#include "libexpr/nixexpr.hh"
#include "libexpr/symbol-table.hh"
#include "libutil/types.hh"

namespace nix {  // TODO(tazjin): ::expr

class EvalState;
struct Value;

/* Map one attribute name to its value. */
struct Attr {
  Symbol name;
  Value* value;  // TODO(tazjin): Who owns this?
  Pos* pos;      // TODO(tazjin): Who owns this?
  Attr(Symbol name, Value* value, Pos* pos = &noPos)
      : name(name), value(value), pos(pos){};
};

using AttributeMap = absl::btree_map<Symbol, Attr>;

class Bindings {
 public:
  typedef AttributeMap::iterator iterator;

  // Allocate a new attribute set that is visible to the garbage
  // collector.
  static Bindings* NewGC(size_t capacity = 0);

  // Create a new attribute set by merging two others. This is used to
  // implement the `//` operator in Nix.
  static Bindings* Merge(const Bindings& lhs, const Bindings& rhs);

  // Return the number of contained elements.
  size_t size() const;

  // Is this attribute set empty?
  bool empty();

  // Insert, but do not replace, values in the attribute set.
  void push_back(const Attr& attr);

  // Look up a specific element of the attribute set.
  iterator find(const Symbol& name);

  iterator begin();
  iterator end();

  // Returns the elements of the attribute set as a vector, sorted
  // lexicographically by keys.
  //
  // This is used primarily for builtins that have guaranteed
  // ordering, such as `attrNames` or `attrValues`.
  std::vector<const Attr*> SortedByKeys();

  // oh no
  friend class EvalState;

 private:
  AttributeMap attributes_;
};

}  // namespace nix

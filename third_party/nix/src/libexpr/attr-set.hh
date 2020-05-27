// This file implements the underlying structure of Nix attribute sets.
#pragma once

#include <absl/container/btree_map.h>
#include <gc/gc_allocator.h>

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
  Attr() : pos(&noPos){};
  bool operator<(const Attr& other) const { return name < other.name; }
};

// TODO: remove this, it only exists briefly while I get rid of the
// current Attr struct
inline bool operator==(const Attr& lhs, const Attr& rhs) {
  return lhs.name == rhs.name;
}

// Convenience alias for the backing map, with the garbage-collecting
// allocator explicitly specified.
using AttributeMap =
    absl::btree_map<Symbol, Attr, std::less<Symbol>,
                    gc_allocator<std::pair<const Symbol, Attr>>>;

class Bindings {
 public:
  typedef AttributeMap::iterator iterator;

  // Allocate a new attribute set that is visible to the garbage
  // collector.
  static Bindings* NewGC();

  // Return the number of contained elements.
  size_t size();

  // Is this attribute set empty?
  bool empty();

  // Insert, but do not replace, values in the attribute set.
  void push_back(const Attr& attr);

  // Insert a value, or replace an existing one.
  void insert_or_assign(const Attr& attr);

  // Look up a specific element of the attribute set.
  iterator find(const Symbol& name);

  // TODO
  iterator begin();
  iterator end();

  // Merge values from other into the current attribute
  void merge(const Bindings& other);

  // ???
  [[deprecated]] size_t capacity();

  // oh no
  // Attr& operator[](size_t pos); //  { return attrs[pos]; }

  // TODO: can callers just iterate?
  [[deprecated]] std::vector<const Attr*> lexicographicOrder();

  // oh no
  friend class EvalState;

 private:
  AttributeMap attributes_;
};

}  // namespace nix

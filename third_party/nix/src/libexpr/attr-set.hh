// This file implements the underlying structure of Nix attribute sets.
#pragma once

#include <functional>
#define IMMER_HAS_LIBGC 1

#include <immer/heap/gc_heap.hpp>
#include <immer/heap/heap_policy.hpp>
#include <immer/map.hpp>
#include <immer/memory_policy.hpp>
#include <immer/refcount/no_refcount_policy.hpp>
#include <immer/transience/gc_transience_policy.hpp>

#include <absl/container/btree_map.h>
#include <absl/hash/hash.h>
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
  Value* value;
  Pos* pos;
  Attr(Symbol name, Value* value, Pos* pos = &noPos)
      : name(name), value(value), pos(pos){};
};

// Immer memory policy for using Boehm GC.
using GcPolicy = immer::memory_policy<immer::heap_policy<immer::gc_heap>,
                                      immer::no_refcount_policy,
                                      immer::gc_transience_policy, false>;

using AttributeMap = immer::map<Symbol, Attr, absl::Hash<Symbol>,
                                std::equal_to<Symbol>, GcPolicy>;

class Bindings {
 public:
  typedef AttributeMap::const_iterator iterator;

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
  const Attr* find(const Symbol& name);

  iterator begin();
  iterator end();

  // TODO: can callers just iterate?
  [[deprecated]] std::vector<const Attr*> lexicographicOrder();

  // oh no
  friend class EvalState;

 private:
  AttributeMap attributes_;
};

}  // namespace nix

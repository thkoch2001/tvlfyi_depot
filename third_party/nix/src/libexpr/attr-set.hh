// This file implements the underlying structure of Nix attribute sets.
#pragma once

#include <cstddef>

#include <absl/container/btree_map.h>
#include <absl/container/flat_hash_map.h>
#include <gc/gc_allocator.h>

#include "libexpr/nixexpr.hh"
#include "libexpr/symbol-table.hh"
#include "libutil/types.hh"

namespace nix {  // TODO(tazjin): ::expr

void load_capacity_pivot();

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

// Convenience alias for the backing map, with the garbage-collecting
// allocator explicitly specified.
using AttributeMap = absl::flat_hash_map<
  Symbol, Attr,
  absl::container_internal::hash_default_hash<Symbol>,
  absl::container_internal::hash_default_eq<Symbol>,
  gc_allocator<std::pair<Symbol, Attr>>>;

using AttributeVector =
    std::vector<std::pair<Symbol, Attr>, gc_allocator<std::pair<Symbol, Attr>>>;

class BindingsIterator : public std::iterator<std::forward_iterator_tag,
                                              std::pair<const Symbol, Attr>> {
  friend class Bindings;
  friend class BTreeBindings;
  friend class VectorBindings;

 public:
  BindingsIterator() : _iterator(){};
  BindingsIterator& operator++();
  BindingsIterator operator++(int);
  bool operator==(const BindingsIterator& other) const;
  bool operator!=(const BindingsIterator& other) const;
  reference operator*() const;
  pointer operator->() const { return &operator*(); }

  BindingsIterator& operator=(const BindingsIterator& other) {
    _iterator = other._iterator;
    return *this;
  }

 protected:
  explicit BindingsIterator(AttributeMap::iterator&& iterator)
      : _iterator(iterator){};

  explicit BindingsIterator(AttributeVector::iterator&& iterator)
      : _iterator(iterator){};

 private:
  std::variant<AttributeMap::iterator, AttributeVector::iterator> _iterator;
};

class Bindings {
 public:
  typedef BindingsIterator iterator;

  // Allocate a new attribute set with a static capacity that is visible to the
  // garbage collector.
  static Bindings* NewGC(size_t capacity = 0);

  // Return the number of contained elements.
  virtual size_t size() = 0;

  // Is this attribute set empty?
  virtual bool empty() = 0;

  // Insert, but do not replace, values in the attribute set.
  virtual void push_back(const Attr& attr) = 0;

  // Look up a specific element of the attribute set.
  virtual iterator find(const Symbol& name) = 0;

  // TODO
  virtual iterator begin() = 0;
  virtual iterator end() = 0;

  // Merge values from other into this attribute set.
  virtual void merge(Bindings& other) = 0;

  // TODO: can callers just iterate?
  [[deprecated]] virtual std::vector<const Attr*> lexicographicOrder() = 0;

  // oh no
  friend class EvalState;
};

}  // namespace nix

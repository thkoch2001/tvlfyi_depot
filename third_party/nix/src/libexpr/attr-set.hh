// This file implements the underlying structure of Nix attribute sets.
#pragma once

#include <absl/container/btree_map.h>

#include "nixexpr.hh"
#include "symbol-table.hh"
#include "types.hh"  // TODO(tazjin): audit this include

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
  bool operator<(const Attr& a) const { return name < a.name; }
};

// TODO: remove this, it only exists briefly while I get rid of the
// current Attr struct
inline bool operator==(const Attr& lhs, const Attr& rhs) {
  return lhs.name == rhs.name;
}

class Bindings {
 public:
  typedef Attr* iterator;  // TODO: type, and also 'using'?

  // Return the number of contained elements.
  size_t size();

  // Is this attribute set empty?
  bool empty();

  // TODO(tazjin): rename
  // TODO(tazjin): does this need to copy?
  void push_back(const Attr& attr);

  // Look up a specific element of the attribute set.
  iterator find(const Symbol& name);

  // TODO
  iterator begin();
  iterator end();

  // ???
  [[deprecated]] void sort();

  // ???
  [[deprecated]] size_t capacity();

  // oh no
  // Attr& operator[](size_t pos); //  { return attrs[pos]; }

  // TODO: can callers just iterate?
  [[deprecated]] std::vector<const Attr*> lexicographicOrder();

  // oh no
  friend class EvalState;

 private:
  absl::btree_map<Symbol, Attr> attributes_;
};
}  // namespace nix

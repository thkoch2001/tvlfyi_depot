#pragma once

#include <absl/container/node_hash_set.h>
#include <absl/strings/string_view.h>

namespace nix {  // TODO(tazjin): ::expr

// TODO(tazjin): Replace with a simpler struct, or get rid of.
class Symbol {
 private:
  const std::string* s;  // pointer into SymbolTable
  Symbol(const std::string* s) : s(s){};
  friend class SymbolTable;

 public:
  Symbol() : s(0){};

  bool operator==(const Symbol& s2) const { return s == s2.s; }

  bool operator!=(const Symbol& s2) const { return s != s2.s; }

  bool operator<(const Symbol& s2) const { return s < s2.s; }

  operator const std::string&() const { return *s; }

  bool set() const { return s; }

  bool empty() const { return s->empty(); }

  friend std::ostream& operator<<(std::ostream& str, const Symbol& sym);
};

// SymbolTable is a hash-set based symbol-interning mechanism.
//
// TODO(tazjin): Figure out which things use this. AttrSets, ...?
// Is it possible this only exists because AttrSet wasn't a map?
//
// Original comment:
//
// Symbol table used by the parser and evaluator to represent and look
// up identifiers and attributes efficiently. SymbolTable::create()
// converts a string into a symbol. Symbols have the property that
// they can be compared efficiently (using a pointer equality test),
// because the symbol table stores only one copy of each string.
class SymbolTable {
 public:
  // Create a new symbol in this table by emplacing the provided
  // string into it.
  //
  // The symbol will reference an existing symbol if the symbol is
  // already interned.
  Symbol Create(absl::string_view sym);

  // Return the number of symbols interned.
  size_t Size() const;

  // Return the total size (in bytes)
  size_t TotalSize() const;

 private:
  // flat_hash_set does not retain pointer stability on rehashing,
  // hence "interned" strings/symbols are stored on the heap.
  absl::node_hash_set<std::string> symbols_;
};

}  // namespace nix

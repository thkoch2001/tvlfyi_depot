#include "symbol-table.hh"

#include <absl/container/node_hash_set.h>
#include <absl/strings/string_view.h>

namespace nix {

Symbol SymbolTable::Create(absl::string_view sym) {
  auto it = symbols_.emplace(sym);
  const string* ptr = &(*it.first);
  return Symbol(ptr);
}

size_t SymbolTable::Size() const { return symbols_.size(); }

size_t SymbolTable::TotalSize() const {
  size_t n = 0;
  for (auto& i : symbols_) {
    n += i.size();
  }
  return n;
}

}  // namespace nix

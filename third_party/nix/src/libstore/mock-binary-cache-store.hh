#pragma once

#include "libstore/binary-cache-store.hh"

#include <absl/container/btree_map.h>
#include <absl/container/flat_hash_map.h>

namespace nix {

// MockBinaryCacheStore implements a memory-based BinaryCacheStore, for use in
// tests.
class MockBinaryCacheStore : public BinaryCacheStore {
 public:
  MockBinaryCacheStore(const Params& params);

  // Store API

  std::string getUri() override;

  bool fileExists(const std::string& path) override;

  void upsertFile(const std::string& path, const std::string& data,
                  const std::string& mimeType) override;

  void getFile(
      const std::string& path,
      Callback<std::shared_ptr<std::string>> callback) noexcept override;

  PathSet queryAllValidPaths() override;

  // Test API

  // Remove a file from the store.
  void deleteFile(const std::string& path);

  // Same as upsert, but bypasses injected errors.
  void setFileContentsForTest(const std::string& path, const std::string& data,
                              const std::string& mimeType);

  void prepareErrorInjection(const std::string& path,
                             std::function<void()> throw_func);

  void cancelErrorInjection(const std::string& path);

  // Internals

 private:
  void throwInjectedErrors(const std::string& path);

  struct MemoryFile {
    std::string data;
    std::string mimeType;
  };

  absl::btree_map<std::string, MemoryFile> contents_;
  absl::flat_hash_map<std::string, std::function<void()>> errorInjections_;
};

}  // namespace nix

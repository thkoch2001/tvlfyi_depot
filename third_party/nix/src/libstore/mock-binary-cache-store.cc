#include "libstore/mock-binary-cache-store.hh"

#include <glog/logging.h>

namespace nix {

MockBinaryCacheStore::MockBinaryCacheStore(const Params& params)
    : BinaryCacheStore(params), contents_(), errorInjections_() {}

std::string MockBinaryCacheStore::getUri() { return "mock://1"; }

bool MockBinaryCacheStore::fileExists(const std::string& path) {
  throwInjectedErrors(path);

  return contents_.find(path) != contents_.end();
};

void MockBinaryCacheStore::upsertFile(const std::string& path,
                                      const std::string& data,
                                      const std::string& mimeType) {
  throwInjectedErrors(path);

  contents_[path] = MemoryFile{data, mimeType};
}

void MockBinaryCacheStore::getFile(
    const std::string& path,
    Callback<std::shared_ptr<std::string>> callback) noexcept {
  auto eit = errorInjections_.find(path);
  if (eit != errorInjections_.end()) {
    try {
      eit->second();
      LOG(FATAL) << "thrower failed to throw";
    } catch (...) {
      callback.rethrow();
    }
    return;
  }

  auto it = contents_.find(path);
  if (it == contents_.end()) {
    try {
      throw NoSuchBinaryCacheFile(absl::StrCat(
          "file '", path, "' was not added to the MockBinaryCache"));
    } catch (...) {
      callback.rethrow();
    }
    return;
  }
  callback(std::make_shared<std::string>(it->second.data));
}

PathSet MockBinaryCacheStore::queryAllValidPaths() {
  PathSet paths;

  for (auto it : contents_) {
    paths.insert(it.first);
  }

  return paths;
}

void MockBinaryCacheStore::deleteFile(const std::string& path) {
  contents_.erase(path);
}

// Same as upsert, but bypasses injected errors.
void MockBinaryCacheStore::setFileContentsForTest(const std::string& path,
                                                  const std::string& data,
                                                  const std::string& mimeType) {
  contents_[path] = MemoryFile{data, mimeType};
}

void MockBinaryCacheStore::prepareErrorInjection(
    const std::string& path, std::function<void()> err_factory) {
  errorInjections_[path] = err_factory;
}

void MockBinaryCacheStore::cancelErrorInjection(const std::string& path) {
  errorInjections_.erase(path);
}

void MockBinaryCacheStore::throwInjectedErrors(const std::string& path) {
  auto it = errorInjections_.find(path);
  if (it != errorInjections_.end()) {
    it->second();
    LOG(FATAL) << "thrower failed to throw";
  }
}

}  // namespace nix

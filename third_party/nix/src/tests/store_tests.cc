#include <filesystem>

#include <absl/container/btree_map.h>
#include <absl/container/flat_hash_map.h>
#include <absl/strings/escaping.h>
#include <glog/logging.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <sys/random.h>

#include "libstore/binary-cache-store.hh"

using ::testing::HasSubstr;

namespace nix {

MakeError(InjectedError, Error);

class StoreTest : public ::testing::Test {
 public:
  static void SetUpTestSuite() {
    google::InitGoogleLogging("--logtostderr=false");
  }
};

class MockBinaryCacheStore : public BinaryCacheStore {
 public:
  MockBinaryCacheStore(const Params& params)
      : BinaryCacheStore(params), contents_(), errorInjections_() {}

  // Store API functions

  std::string getUri() override { return "mock://1"; }

  bool fileExists(const std::string& path) override {
    throwInjectedErrors(path);

    return contents_.find(path) != contents_.end();
  };

  void upsertFile(const std::string& path, const std::string& data,
                  const std::string& mimeType) override {
    throwInjectedErrors(path);

    contents_[path] = MemoryFile{data, mimeType};
  }

  void getFile(
      const std::string& path,
      Callback<std::shared_ptr<std::string>> callback) noexcept override {
    auto eit = errorInjections_.find(path);
    if (eit != errorInjections_.end()) {
      try {
        eit->second();
        FAIL() << "thrower failed to throw";
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

  PathSet queryAllValidPaths() override {
    PathSet paths;

    for (auto it : contents_) {
      paths.insert(it.first);
    }

    return paths;
  }

  // Test API

  void deleteFile(const std::string& path) { contents_.erase(path); }

  // Same as upsert, but bypasses injected errors.
  void setFileContentsForTest(const std::string& path, const std::string& data,
                              const std::string& mimeType) {
    contents_[path] = MemoryFile{data, mimeType};
  }

  void prepareErrorInjection(const std::string& path,
                             std::function<void()> err_factory) {
    errorInjections_[path] = err_factory;
  }

  void cancelErrorInjection(const std::string& path) {
    errorInjections_.erase(path);
  }

  // Internals

 private:
  void throwInjectedErrors(const std::string& path) {
    auto it = errorInjections_.find(path);
    if (it != errorInjections_.end()) {
      it->second();
      FAIL() << "thrower failed to throw";
    }
  }

  struct MemoryFile {
    std::string data;
    std::string mimeType;
  };

  absl::btree_map<std::string, MemoryFile> contents_;
  absl::flat_hash_map<std::string, std::function<void()>> errorInjections_;
};

class BinaryCacheStoreTest : public StoreTest {};

constexpr absl::string_view kXZHeader = "7zXZ";

constexpr absl::string_view kRootFileName = "myRootFile";
constexpr absl::string_view kDep1FileName = "dep1";
constexpr absl::string_view kDep1FileContents = "==dep1 contents==";
constexpr absl::string_view kDep1NarCache =
    "nar/0hfdc95cy6mxi4c15pp0frdf97r7yvd8c141qzvpms2f8x17p2ig.nar.xz";
constexpr absl::string_view kBogusPath =
    "/nix/store/g1ghizdg18k0d00000000000000z3v32-doesNotExist";

struct TestTree {
  Path rootPath;
  Path dep1Path;
};

TestTree AddTestTreeToStore(Store& store) {
  TestTree results;
  results.rootPath =
      store.addTextToStore(std::string(kRootFileName), "1", PathSet());

  PathSet onlyRoot;
  onlyRoot.insert(results.rootPath);
  results.dep1Path = store.addTextToStore(
      std::string(kDep1FileName), std::string(kDep1FileContents), onlyRoot);

  return results;
}

TEST_F(BinaryCacheStoreTest, BasicStorage) {
  MockBinaryCacheStore::Params params;
  MockBinaryCacheStore store(params);

  store.init();

  auto tree = AddTestTreeToStore(store);

  EXPECT_TRUE(store.isValidPath(tree.rootPath));
  EXPECT_TRUE(store.isValidPath(tree.dep1Path));

  StringSink sink;
  store.narFromPath(tree.dep1Path, sink);
  EXPECT_THAT(*sink.s, HasSubstr(kDep1FileContents));

  EXPECT_THAT(*store.BinaryCacheStore::getFile(Path(kDep1NarCache)),
              HasSubstr(kXZHeader));
}

TEST_F(BinaryCacheStoreTest, BasicErrors) {
  MockBinaryCacheStore::Params params;
  MockBinaryCacheStore store(params);

  store.init();

  auto tree = AddTestTreeToStore(store);
  store.prepareErrorInjection(std::string(kDep1NarCache),
                              []() { throw InjectedError("injected"); });

  {
    StringSink sink;
    EXPECT_THROW(store.narFromPath(tree.dep1Path, sink), InjectedError);
  }
  {
    StringSink sink;
    EXPECT_THROW(store.narFromPath(std::string(kBogusPath), sink),
                 NoSuchBinaryCacheFile);
  }
}

}  // namespace nix

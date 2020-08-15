#include <filesystem>

#include <absl/container/btree_map.h>
#include <absl/container/flat_hash_map.h>
#include <absl/strings/escaping.h>
#include <glog/logging.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <sys/random.h>

#include "libstore/binary-cache-store.hh"
#include "libstore/mock-binary-cache-store.hh"
#include "tests/store-util.hh"

using ::testing::HasSubstr;

namespace nix {

MakeError(InjectedError, Error);

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
  store.PrepareErrorInjection(std::string(kDep1NarCache),
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

// ./tests/add.sh
TEST_F(StoreTest, AddFileHashes) {
  auto store_ = OpenTemporaryStore().ConsumeValueOrDie();
  nix::Store* store = static_cast<nix::Store*>(store_.get());
  nix::Path dataPath = NIX_SRC_DIR "/src/tests/lang/data";
  std::string dataName = "data";

  nix::Path path1 = store->addToStore(dataName, dataPath);

  nix::Path path2 = store->addToStore(dataName, dataPath, /*recursive=*/true,
                                      HashType::htSHA256);

  EXPECT_EQ(path1, path2) << "nix-store --add and --add-fixed mismatch";

  nix::Path path3 = store->addToStore(dataName, dataPath, /*recursive=*/false,
                                      HashType::htSHA256);
  EXPECT_NE(path1, path3);

  nix::Path path4 =
      store->addToStore(dataName, dataPath, false, HashType::htSHA1);
  EXPECT_NE(path1, path4);

  auto info1 = store->queryPathInfo(store->followLinksToStorePath(path1));
  ASSERT_EQ(info1->narHash.type, HashType::htSHA256);

  Hash h = nix::hashPath(HashType::htSHA256, dataPath).first;

  EXPECT_EQ(info1->narHash.to_string(), h.to_string());
}

}  // namespace nix

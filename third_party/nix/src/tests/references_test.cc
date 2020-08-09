#include "libstore/references.hh"

#include <cstdio>
#include <fstream>
#include <ostream>
#include <unordered_set>

#include <absl/strings/str_format.h>
#include <gtest/gtest.h>
#include <rapidcheck.h>
#include <rapidcheck/gtest.h>

#include "libutil/hash.hh"

class ReferencesTest : public ::testing::Test {};

namespace nix {

TEST(ReferencesTest, ScanForNoReferences) {
  char path[] = "store_XXXXXX";
  auto f = mkstemp(path);
  ASSERT_EQ(errno, 0);

  HashResult hash;
  auto result = scanForReferences(path, {}, hash);

  ASSERT_TRUE(result.empty());

  EXPECT_EQ(close(f), 0);
}

TEST(ReferencesTest, ScanForOneReferenceNotFound) {
  char path[] = "store_XXXXXXX";
  auto f = mkstemp(path);

  auto hash = hashString(htSHA256, "foo");
  auto ref = absl::StrFormat("/nix/store/%s-foo", hash.ToStorePathHash());

  HashResult hr;
  auto result = scanForReferences(path, {ref}, hr);

  ASSERT_EQ(result.find(ref), result.end());

  EXPECT_EQ(close(f), 0);
}

TEST(ReferencesTest, ScanForOneReferenceFound) {
  char path[] = "store_XXXXXXX";
  auto f = mkstemp(path);

  auto hash = hashString(htSHA256, "foo");
  auto ref = absl::StrFormat("/nix/store/%s-foo", hash.ToStorePathHash());

  EXPECT_GT(write(f, ref.c_str(), sizeof(char) * ref.size()), 0);

  HashResult hr;
  auto result = scanForReferences(path, {ref}, hr);

  ASSERT_NE(result.find(ref), result.end());

  ASSERT_EQ(close(f), 0);
}

RC_GTEST_PROP(ReferencesTest, ScanForReferences,
              (std::unordered_set<std::string> strs)) {
  char path[] = "store_XXXXXXX";
  auto f = mkstemp(path);

  PathSet refs;
  for (const auto& s : strs) {
    auto hash = hashString(htSHA256, s);
    auto ref = absl::StrFormat("/nix/store/%s-foo", hash.ToStorePathHash());
    refs.insert(ref);
    EXPECT_GT(write(f, ref.c_str(), sizeof(char) * ref.size()), 0);
  }

  HashResult hr;
  auto result = scanForReferences(path, refs, hr);

  for (const auto& ref : refs) {
    ASSERT_NE(result.find(ref), result.end());
  }

  EXPECT_EQ(close(f), 0);
}

}  // namespace nix

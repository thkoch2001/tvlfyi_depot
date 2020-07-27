#include "libutil/hash.hh"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

class HashTest : public ::testing::Test {};

using testing::EndsWith;
using testing::HasSubstr;

namespace nix {

TEST(HashTest, SHA256) {
  auto hash = hashString(HashType::htSHA256, "foo");
  ASSERT_EQ(hash.base64Len(), 44);
  ASSERT_EQ(hash.base32Len(), 52);
  ASSERT_EQ(hash.base16Len(), 64);

  ASSERT_EQ(hash.to_string(Base16),
            "sha256:"
            "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae");
  ASSERT_EQ(hash.to_string(Base32),
            "sha256:1bp7cri8hplaz6hbz0v4f0nl44rl84q1sg25kgwqzipzd1mv89ic");
  ASSERT_EQ(hash.to_string(Base64),
            "sha256:LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm564=");
}

TEST(HashTest, SHA256Decode) {
  auto hash = hashString(HashType::htSHA256, "foo");

  std::unique_ptr<Hash> base16 = std::make_unique<Hash>(
      "sha256:"
      "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae",
      HashType::htSHA256);
  std::unique_ptr<Hash> base32 = std::make_unique<Hash>(
      "sha256:1bp7cri8hplaz6hbz0v4f0nl44rl84q1sg25kgwqzipzd1mv89ic",
      HashType::htSHA256);
  std::unique_ptr<Hash> base64 = std::make_unique<Hash>(
      "sha256:LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm564=",
      HashType::htSHA256);

  ASSERT_EQ(hash, *base16);
  ASSERT_EQ(hash, *base32);
  ASSERT_EQ(hash, *base64);
}

TEST(HashTest, SHA256DecodeFail) {
  EXPECT_THAT(
      Hash::deserialize("sha256:LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm56==",
                        HashType::htSHA256)
          .status()
          .message(),
      HasSubstr("wrong length"));
  EXPECT_THAT(
      Hash::deserialize("sha256:LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm56,=",
                        HashType::htSHA256)
          .status()
          .message(),
      HasSubstr("invalid base-64"));

  EXPECT_THAT(Hash::deserialize(
                  "sha256:1bp7cri8hplaz6hbz0v4f0nl44rl84q1sg25kgwqzipzd1mv89i",
                  HashType::htSHA256)
                  .status()
                  .message(),
              HasSubstr("wrong length"));
  absl::StatusOr<Hash> badB32Char = Hash::deserialize(
      "sha256:1bp7cri8hplaz6hbz0v4f0nl44rl84q1sg25kgwqzipzd1mv89i,",
      HashType::htSHA256);
  EXPECT_THAT(badB32Char.status().message(), HasSubstr("invalid base-32"));
  EXPECT_THAT(badB32Char.status().message(), EndsWith(","));

  EXPECT_THAT(
      Hash::deserialize(
          "sha256:"
          "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7  ",
          HashType::htSHA256)
          .status()
          .message(),
      HasSubstr("invalid base-16"));
}

}  // namespace nix

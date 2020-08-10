#include <filesystem>

#include <glog/logging.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "libutil/util.hh"
#include "tests/status_helpers.h"

using ::testing::HasSubstr;
using ::testing::IsStatusCode;

namespace nix {

class FileTests : public ::testing::Test {
 public:
  static void SetUpTestSuite() {
    char dirTmpl[] = "nix_test_XXXXXX";
    char* dir = mkdtemp(dirTmpl);
    if (!dir) {
      FAIL() << "could not create temporary directory: " << strerror(errno);
    }
    testDirectory = dir;
  }

  static void TearDownTestSuite() {
    std::filesystem::remove_all(testDirectory);
  }

 protected:
  static std::filesystem::path testDirectory;
};

std::filesystem::path FileTests::testDirectory = "";

TEST_F(FileTests, ReadRegularFile) {
  constexpr absl::string_view kFileContents =
      "regular file contents\n123 456 789";
  auto path = testDirectory / "regular_file";
  nix::writeFile(path.string(), std::string(kFileContents), 0600);

  auto contents = nix::readFile_Status(path.string());
  EXPECT_OK(contents);
  EXPECT_EQ(*contents, kFileContents);
}

TEST_F(FileTests, ReadDevNull) {
  auto contents = nix::readFile_Status("/dev/null");
  EXPECT_OK(contents);
  EXPECT_EQ(*contents, "");
}

TEST_F(FileTests, ReadDoesNotExist) {
  auto path = testDirectory / "does_not_exist";
  auto contents = nix::readFile_Status(path.string());
  EXPECT_THAT(contents, IsStatusCode(absl::StatusCode::kNotFound));
}

}  // namespace nix

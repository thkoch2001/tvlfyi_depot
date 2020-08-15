#pragma once

#include <filesystem>

#include <absl/status/statusor.h>
#include <absl/strings/escaping.h>

#include "libstore/local-store.hh"

class StoreTest : public ::testing::Test {
 public:
  static void SetUpTestSuite() {
    google::InitGoogleLogging("--logtostderr=false");
  }

  virtual void TearDown() {
    for (auto fn : cleanup_funcs_) {
      try {
        fn();
      } catch (std::exception e) {
        LOG(ERROR) << e.what();
      }
    }
  }

  absl::StatusOr<std::filesystem::path> OpenTempDir(
      std::filesystem::path parent = std::filesystem::temp_directory_path()) {
    while (1) {
      constexpr int kByteCnt = 9;
      std::string randBytes;
      randBytes.reserve(kByteCnt);
      if (getrandom(randBytes.data(), kByteCnt, 0) < 0) {
        return absl::InternalError("getrandom() failed");
      }

      std::error_code ec;
      std::filesystem::path candidate =
          parent /
          absl::StrCat("nixtest-", absl::WebSafeBase64Escape(randBytes));
      if (std::filesystem::create_directory(candidate, ec)) {
        cleanup_funcs_.push_back(
            [candidate]() { std::filesystem::remove_all(candidate); });
        return candidate;
      }
    }
  }

  absl::StatusOr<std::unique_ptr<nix::LocalStore>> OpenTemporaryStore() {
    ASSIGN_OR_RETURN(std::filesystem::path storePath, OpenTempDir());

    nix::Store::Params params;
    params["root"] = storePath;

    return std::make_unique<nix::LocalStore>(params);
  }

 private:
  std::vector<std::function<void(void)>> cleanup_funcs_;
};

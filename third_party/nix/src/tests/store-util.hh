#pragma once

#include <filesystem>

#include <absl/status/statusor.h>
#include <absl/strings/escaping.h>
#include <glog/logging.h>
#include <sys/random.h>

#include "libstore/local-store.hh"

namespace nix {

class StoreTest : public ::testing::Test {
 public:
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
    for (;;) {
      constexpr int kByteCnt = 9;
      std::array<char, kByteCnt> randBytes;
      if (getrandom(randBytes.data(), kByteCnt, 0) < 0) {
        return absl::InternalError("getrandom() failed");
      }
      std::string suffix = absl::WebSafeBase64Escape(
          absl::string_view(randBytes.data(), kByteCnt));
      CHECK(suffix != "");

      // Workaround for stdlib bug: use .assign() and ::errc
      // https://stackoverflow.com/a/52401295/1210278
      std::error_code ec_exists;
      ec_exists.assign(EEXIST, std::system_category());

      std::error_code ec;
      std::filesystem::path candidate =
          parent / absl::StrCat("nixtest-", suffix);
      if (std::filesystem::create_directory(candidate, ec)) {
        cleanup_funcs_.push_back(
            [candidate]() { std::filesystem::remove_all(candidate); });
        return candidate;
      } else if (ec == ec_exists || ec == std::errc::file_exists) {
        // Directory existed, retry
        continue;
      } else {
        return absl::InternalError(absl::StrCat(
            "could not create dir ", candidate.c_str(), ": ", ec.message()));
      }
    }
  }

  absl::StatusOr<std::unique_ptr<nix::LocalStore>> OpenTemporaryStore() {
    absl::StatusOr<std::filesystem::path> storePath = OpenTempDir();
    if (!storePath.ok()) {
      return storePath.status();
    }

    nix::Store::Params params;
    params["root"] = *storePath;

    return std::make_unique<nix::LocalStore>(params);
  }

 private:
  std::vector<std::function<void(void)>> cleanup_funcs_;
};

}  // namespace nix

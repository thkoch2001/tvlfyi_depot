#pragma once

#include <absl/status/status.h>
#include <absl/status/statusor.h>
#include <absl/strings/str_cat.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>

namespace testing {
namespace nix_internal {

using ::testing::MakeMatcher;
using ::testing::Matcher;
using ::testing::MatcherInterface;
using ::testing::MatchResultListener;

MATCHER_P(IsStatusCode, code, "") { return arg.code() == code; }

class StatusCodeMatcher {
 public:
  StatusCodeMatcher(absl::StatusCode code) : code_(code) {}

  template <class T>
  bool MatchAndExplain(const T& status,
                       MatchResultListener* /* listener */) const {
    return status.code() == code_;
  }

  template <class U>
  bool MatchAndExplain(absl::StatusOr<U>* statusor,
                       MatchResultListener* /* listener */) const {
    return statusor->status().code() == code_;
  }

  void DescribeTo(std::ostream* os) const { *os << "is " << code_; }

  void DescribeNegationTo(std::ostream* os) const { *os << "isn't " << code_; }

 private:
  absl::StatusCode code_;
};

}  // namespace nix_internal

PolymorphicMatcher<nix_internal::StatusCodeMatcher> IsStatusCode(
    absl::StatusCode code) {
  return MakePolymorphicMatcher(nix_internal::StatusCodeMatcher(code));
}

#define EXPECT_OK(status) \
  EXPECT_THAT((status), testing::IsStatusCode(absl::StatusCode::kOk))

}  // namespace testing

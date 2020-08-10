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

  // Match on absl::Status.
  template <class T, typename std::enable_if<std::is_same<T, absl::Status>::value, int>::type int_ = 0>
  bool MatchAndExplain(const T& status,
                       MatchResultListener* /* listener */) const {
    return status.code() == code_;
  }

  // Match on absl::StatusOr.
  //
  // note: I check for the return value of ConsumeValueOrDie because it's the
  // only non-overloaded member I could figure out how to select. Checking for
  // the presence of .status() didn't work because it's overloaded, so
  // std::invoke_result can't pick which overload to use.
  template <class T, typename std::enable_if<std::is_same<typename std::invoke_result<decltype(&T::ConsumeValueOrDie), T>::type, typename T::value_type>::value, int>::type int_ = 0>
  bool MatchAndExplain(const T& statusor,
                       MatchResultListener* /* listener */) const {
    return statusor.status().code() == code_;
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

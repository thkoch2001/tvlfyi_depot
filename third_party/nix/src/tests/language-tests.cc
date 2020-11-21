// This file defines the language test suite. Language tests are run
// by evaluating a small snippet of Nix code (against a fake store),
// serialising it to a string and comparing that to a known output.
//
// This test suite is a port of the previous language integration test
// suite, and it's previous structure is retained.
//
// Test cases are written in nix files under lang/, following one of
// four possible filename patterns which trigger different behaviours:
//
// 1. parse-fail-*.nix: These files contain expressions which should
//    cause a parser failure.
//
// 2. parse-okay-*.nix: These files contain expressions which should
//    parse fine.
//
// 3. eval-fail-*.nix: These files contain expressions which should
//    parse, but fail to evaluate.
//
// 4. eval-okay-*.nix: These files contain expressions which should
//    parse and evaluate fine. They have accompanying .exp files which
//    contain the expected string representation of the evaluation.

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <optional>
#include <sstream>
#include <string>

#include <absl/strings/ascii.h>
#include <absl/strings/match.h>
#include <absl/strings/str_cat.h>
#include <absl/strings/str_split.h>
#include <absl/strings/string_view.h>
#include <glog/logging.h>
#include <gtest/gtest-param-test.h>
#include <gtest/gtest.h>
#include <gtest/internal/gtest-param-util.h>

#include "libexpr/eval-inline.hh"
#include "libexpr/eval.hh"
#include "libexpr/nixexpr.hh"
#include "nix_config.h"
#include "tests/dummy-store.hh"
#include "tests/store-util.hh"

namespace nix::tests {
namespace {

// List all the language test .nix files matching the given prefix.
std::vector<std::filesystem::path> TestFilesFor(absl::string_view prefix) {
  std::vector<std::filesystem::path> matching_files;

  auto dir_iter =
      std::filesystem::directory_iterator(NIX_SRC_DIR "/src/tests/lang");

  for (auto& entry : dir_iter) {
    if (!entry.is_regular_file()) {
      continue;
    }

    auto filename = entry.path().filename().string();
    if (absl::StartsWith(filename, prefix) &&
        absl::EndsWith(filename, ".nix")) {
      matching_files.push_back(entry.path());
    }
  }

  std::sort(matching_files.begin(), matching_files.end());
  return matching_files;
}

// Construct a test name from a path parameter, re-casing its name to
// PascalCase. Googletest only accepts alphanumeric test-names, but
// the file names are in kebab-case.
std::string TestNameFor(
    const testing::TestParamInfo<std::filesystem::path>& info) {
  std::string name;

  for (auto part :
       absl::StrSplit(info.param.stem().string(), '-', absl::SkipEmpty())) {
    std::string part_owned(part);
    part_owned[0] = absl::ascii_toupper(part_owned[0]);
    absl::StrAppend(&name, part_owned);
  }

  return name;
}

// Load the expected output of a given test as a string.
std::string ExpectedOutputFor(absl::string_view stem) {
  std::filesystem::path path(
      absl::StrCat(NIX_SRC_DIR, "/src/tests/lang/", stem, ".exp"));

  EXPECT_TRUE(std::filesystem::exists(path))
      << stem << ": expected output file should exist";

  std::ifstream input(path);
  std::stringstream buffer;
  buffer << input.rdbuf();
  return std::string(absl::StripTrailingAsciiWhitespace(buffer.str()));
}

}  // namespace

using nix::tests::DummyStore;

class NixEnvironment : public testing::Environment {
 public:
  void SetUp() override {
    google::InitGoogleLogging("--logtostderr=false");
    nix::expr::InitGC();
  }
};

::testing::Environment* const nix_env =
    ::testing::AddGlobalTestEnvironment(new NixEnvironment);

class ParserFailureTest : public testing::TestWithParam<std::filesystem::path> {
};

// Test pattern for files that should fail to parse.
TEST_P(ParserFailureTest, Fails) {
  std::shared_ptr<Store> store = std::make_shared<DummyStore>();
  EvalState state({}, ref<Store>(store));
  auto path = GetParam();

  // There are multiple types of exceptions that the parser can throw,
  // and the tests don't define which one they expect, so we need to
  // allow all of these - but fail on other errors.
  try {
    state.parseExprFromFile(GetParam().string());
    FAIL() << path.stem().string() << ": parsing should not succeed";
  } catch (ParseError e) {
    SUCCEED();
  } catch (UndefinedVarError e) {
    SUCCEED();
  } catch (const std::exception& e) {
    FAIL() << path.stem().string()
           << ": unexpected parser exception: " << e.what();
  }
}

INSTANTIATE_TEST_SUITE_P(Parser, ParserFailureTest,
                         testing::ValuesIn(TestFilesFor("parse-fail-")),
                         TestNameFor);

class ParserSuccessTest : public testing::TestWithParam<std::filesystem::path> {
};

// Test pattern for files that should parse successfully.
TEST_P(ParserSuccessTest, Parses) {
  std::shared_ptr<Store> store = std::make_shared<DummyStore>();
  EvalState state({}, ref<Store>(store));
  auto path = GetParam();

  EXPECT_NO_THROW(state.parseExprFromFile(GetParam().string()))
      << path.stem().string() << ": parsing should succeed";

  SUCCEED();
}

INSTANTIATE_TEST_SUITE_P(Parser, ParserSuccessTest,
                         testing::ValuesIn(TestFilesFor("parse-okay-")),
                         TestNameFor);

class EvalFailureTest : public testing::TestWithParam<std::filesystem::path> {};

// Test pattern for files that should fail to evaluate.
TEST_P(EvalFailureTest, Fails) {
  std::shared_ptr<Store> store = std::make_shared<DummyStore>();
  EvalState state({}, ref<Store>(store));
  auto path = GetParam();

  Expr* expr = nullptr;
  EXPECT_NO_THROW(expr = state.parseExprFromFile(GetParam().string()))
      << path.stem().string() << ": should parse successfully";

  // Again, there are multiple expected exception types and the tests
  // don't specify which ones they are looking for.
  try {
    Value result;
    state.eval(expr, result);
    state.forceValue(result);
    std::cout << result;
    FAIL() << path.stem().string() << ": evaluating should not succeed";
  } catch (AssertionError e) {
    SUCCEED();
  } catch (EvalError e) {
    SUCCEED();
  } catch (SysError e) {
    SUCCEED();
  } catch (ParseError /* sic! */ e) {
    SUCCEED();
  } catch (const std::exception& e) {
    FAIL() << path.stem().string()
           << ": unexpected evaluator exception: " << e.what();
  }
}

INSTANTIATE_TEST_SUITE_P(Eval, EvalFailureTest,
                         testing::ValuesIn(TestFilesFor("eval-fail-")),
                         TestNameFor);

class EvalSuccessTest : public testing::TestWithParam<std::filesystem::path> {};

// Test pattern for files that should evaluate successfully.
TEST_P(EvalSuccessTest, Succeeds) {
  std::shared_ptr<Store> store = std::make_shared<DummyStore>();
  EvalState state({}, ref<Store>(store));
  auto path = GetParam();

  Expr* expr = nullptr;
  ASSERT_NO_THROW(expr = state.parseExprFromFile(GetParam().string()))
      << path.stem().string() << ": should parse successfully";

  Value result;

  ASSERT_NO_THROW({
    state.eval(expr, result);
    state.forceValueDeep(result);
  }) << path.stem().string()
     << ": should evaluate successfully";

  auto expected = ExpectedOutputFor(path.stem().string());
  std::ostringstream value_str;
  value_str << result;

  EXPECT_EQ(expected, value_str.str()) << "evaluator output should match";
}

INSTANTIATE_TEST_SUITE_P(Eval, EvalSuccessTest,
                         testing::ValuesIn(TestFilesFor("eval-okay-")),
                         TestNameFor);

class BlankStoreTest : public nix::StoreTest {
  virtual void TestBody() override{};
};

class EvalStoreSuccessTest
    : public testing::TestWithParam<std::filesystem::path> {
 public:
  virtual void TearDown() { store_test_.TearDown(); }

  absl::StatusOr<std::unique_ptr<nix::LocalStore>> OpenTemporaryStore() {
    return store_test_.OpenTemporaryStore();
  }

 private:
  BlankStoreTest store_test_;
};

// Test pattern for files that should evaluate successfully but require a real
// store.
TEST_P(EvalStoreSuccessTest, Succeeds) {
  absl::StatusOr<std::unique_ptr<nix::LocalStore>> store_ =
      OpenTemporaryStore();
  CHECK(store_.ok()) << "failed to open temporary store";
  ref<Store> store = ref<Store>(store_->release());
  EvalState state({}, store);
  auto path = GetParam();

  Expr* expr = nullptr;
  ASSERT_NO_THROW(expr = state.parseExprFromFile(GetParam().string()))
      << path.stem().string() << ": should parse successfully";

  Value result;

  ASSERT_NO_THROW({
    state.eval(expr, result);
    state.forceValueDeep(result);
  }) << path.stem().string()
     << ": should evaluate successfully";

  auto expected = ExpectedOutputFor(path.stem().string());
  std::ostringstream value_str;
  value_str << result;

  EXPECT_EQ(expected, value_str.str()) << "evaluator output should match";
}

INSTANTIATE_TEST_SUITE_P(Eval, EvalStoreSuccessTest,
                         testing::ValuesIn(TestFilesFor("evalstore-okay-")),
                         TestNameFor);

}  // namespace nix::tests

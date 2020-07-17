#include "libexpr/value-to-json.hh"

#include <set>
#include <sstream>

#include <gtest/gtest.h>

#include "libexpr/value-to-xml.hh"
#include "libexpr/value.hh"
#include "libstore/store-api.hh"

class ValueTest : public ::testing::Test {
 protected:
  static void SetUpTestCase() { nix::initGC(); }

  static void TearDownTestCase() {}
};

class JSONValueTest : public ValueTest {};
class XMLValueTest : public ValueTest {};

namespace nix {

class DummyStore final : public Store {
 public:
  explicit DummyStore() : Store(Store::Params{}) {}

  std::string getUri() { return ""; }

  void queryPathInfoUncached(
      const Path& path,
      Callback<std::shared_ptr<ValidPathInfo>> callback) noexcept {}

  Path queryPathFromHashPart(const std::string& hashPart) { return ""; }

  Path addToStore(const std::string& name, const Path& srcPath,
                  bool recursive = true, HashType hashAlgo = htSHA256,
                  PathFilter& filter = defaultPathFilter,
                  RepairFlag repair = NoRepair) {
    return "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x";
  }

  Path addTextToStore(const std::string& name, const std::string& s,
                      const PathSet& references, RepairFlag repair = NoRepair) {
    return "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x";
  }

  void narFromPath(const Path& path, Sink& sink) {}

  BuildResult buildDerivation(const Path& drvPath, const BasicDerivation& drv,
                              BuildMode buildMode = bmNormal) {
    return BuildResult{};
  }

  void ensurePath(const Path& path) {}
};

TEST_F(JSONValueTest, null) {
  std::stringstream ss;
  Value v;
  PathSet ps;
  std::shared_ptr<Store> store = std::make_shared<DummyStore>();
  EvalState s({}, ref<Store>(store));

  mkNull(v);
  printValueAsJSON(s, true, v, ss, ps);
  ASSERT_EQ(ss.str(), "null");
}

TEST_F(JSONValueTest, BoolFalse) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkBool(v, false);
  printValueAsJSON(s, true, v, ss, ps);
  ASSERT_EQ(ss.str(), "false");
}

TEST_F(JSONValueTest, BoolTrue) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkBool(v, true);
  printValueAsJSON(s, true, v, ss, ps);
  ASSERT_EQ(ss.str(), "true");
}

TEST_F(JSONValueTest, IntPositive) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkInt(v, 100);
  printValueAsJSON(s, true, v, ss, ps);
  ASSERT_EQ(ss.str(), "100");
}

TEST_F(JSONValueTest, IntNegative) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkInt(v, -100);
  printValueAsJSON(s, true, v, ss, ps);
  ASSERT_EQ(ss.str(), "-100");
}

TEST_F(JSONValueTest, String) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkString(v, "test");
  printValueAsJSON(s, true, v, ss, ps);
  ASSERT_EQ(ss.str(), "\"test\"");
}

TEST_F(JSONValueTest, StringQuotes) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkString(v, "test\"");
  printValueAsJSON(s, true, v, ss, ps);
  ASSERT_EQ(ss.str(), "\"test\\\"\"");
}

TEST_F(JSONValueTest, Path) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkPath(v, "test");
  printValueAsJSON(s, true, v, ss, ps);
  ASSERT_EQ(ss.str(), "\"/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x\"");
}

TEST_F(JSONValueTest, PathNoCopy) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkPathNoCopy(v, "test");
  printValueAsJSON(s, true, v, ss, ps);
  ASSERT_EQ(ss.str(), "\"/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x\"");
}

/*
 * Value to XMl tests
 */

#define XML(v) \
  ("<?xml version='1.0' encoding='utf-8'?>\n<expr>\n" v "\n</expr>\n")

TEST_F(XMLValueTest, null) {
  std::stringstream ss;
  Value v;
  PathSet ps;
  auto store = std::make_shared<DummyStore>();
  EvalState s({}, ref<Store>(store));

  mkNull(v);
  printValueAsXML(s, true, true, v, ss, ps);
  ASSERT_EQ(ss.str(), XML("  <null />"));
}

TEST_F(XMLValueTest, BoolFalse) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkBool(v, false);
  printValueAsXML(s, true, true, v, ss, ps);
  ASSERT_EQ(ss.str(), XML("  <bool value=\"false\" />"));
}

TEST_F(XMLValueTest, BoolTrue) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkBool(v, true);
  printValueAsXML(s, true, true, v, ss, ps);
  ASSERT_EQ(ss.str(), XML("  <bool value=\"true\" />"));
}

TEST_F(XMLValueTest, IntPositive) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkInt(v, 100);
  printValueAsXML(s, true, true, v, ss, ps);
  ASSERT_EQ(ss.str(), XML("  <int value=\"100\" />"));
}

TEST_F(XMLValueTest, IntNegative) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkInt(v, -100);
  printValueAsXML(s, true, true, v, ss, ps);
  ASSERT_EQ(ss.str(), XML("  <int value=\"-100\" />"));
}

TEST_F(XMLValueTest, String) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkString(v, "test-value");
  printValueAsXML(s, true, true, v, ss, ps);
  ASSERT_EQ(ss.str(), XML("  <string value=\"test-value\" />"));
}

TEST_F(XMLValueTest, StringQuotes) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkString(v, "test-value\"");
  printValueAsXML(s, true, true, v, ss, ps);
  ASSERT_EQ(ss.str(), XML("  <string value=\"test-value&quot;\" />"));
}

/*
 * FIXME: This function returns the original input path while the JSON version
 * of the same actually touches the store to create a /nix/store path.
 */
TEST_F(XMLValueTest, Path) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkPath(v, "some-path");
  printValueAsXML(s, true, true, v, ss, ps);
  ASSERT_EQ(ss.str(), XML("  <path value=\"some-path\" />"));
}

/*
 * FIXME: This function returns the original input path while the JSON version
 * of the same actually touches the store to create a /nix/store path.
 */
TEST_F(XMLValueTest, PathNoCopy) {
  std::stringstream ss;
  auto store = std::make_shared<DummyStore>();
  EvalState s({"."}, ref<Store>(store));
  Value v;
  PathSet ps;

  mkPathNoCopy(v, "some-other-path");
  printValueAsXML(s, true, true, v, ss, ps);
  ASSERT_EQ(ss.str(), XML("  <path value=\"some-other-path\" />"));
}
}  // namespace nix

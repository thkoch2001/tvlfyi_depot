#include "libexpr/attr-set.hh"

#include <gtest/gtest.h>
#include <rapidcheck.h>
#include <rapidcheck/gtest.h>

class AttrSetTest : public ::testing::Test {
  static void SetUpTestCase() {}
  static void TearDownTestCase() {}
};

namespace nix {
  RC_GTEST_PROP(AttrSetTest, inRangeValueIsInRange, ()) {
    const auto range = *rc::gen::arbitrary<std::pair<int, int>>();
    const auto x = *rc::gen::inRange(range.first, range.second);
    RC_ASSERT(x >= range.first);
    RC_ASSERT(x < range.second);
  }

}  // namespace nix

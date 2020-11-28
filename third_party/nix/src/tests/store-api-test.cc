#include "libstore/store-api.hh"

#include <gtest/gtest.h>
#include <rapidcheck/Assertions.h>
#include <rapidcheck/gtest.h>

#include "libproto/worker.pb.h"
#include "tests/arbitrary.hh"

namespace nix {

class BuildResultTest : public ::testing::Test {};

RC_GTEST_PROP(BuildResultTest, StatusToFromProtoRoundTrip,
              (BuildResult::Status && status)) {
  BuildResult br;
  br.status = status;

  auto proto_status = br.status_to_proto();
  nix::proto::BuildResult br_proto;
  br_proto.set_status(proto_status);

  auto result = BuildResult::FromProto(br_proto);

  RC_ASSERT(result.value().status == status);
}

}  // namespace nix

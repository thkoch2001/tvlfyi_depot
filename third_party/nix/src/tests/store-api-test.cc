#include "libstore/store-api.hh"

#include <gtest/gtest.h>
#include <rapidcheck/Assertions.h>
#include <rapidcheck/Gen.h>
#include <rapidcheck/gtest.h>

#include "libproto/worker.pb.h"

namespace rc {
template <>
struct Arbitrary<nix::BuildResult::Status> {
  static Gen<nix::BuildResult::Status> arbitrary() {
    return gen::element(nix::BuildResult::Status::Built,
                        nix::BuildResult::Status::Substituted,
                        nix::BuildResult::Status::AlreadyValid,
                        nix::BuildResult::Status::PermanentFailure,
                        nix::BuildResult::Status::InputRejected,
                        nix::BuildResult::Status::OutputRejected,
                        nix::BuildResult::Status::TransientFailure,
                        nix::BuildResult::Status::CachedFailure,
                        nix::BuildResult::Status::TimedOut,
                        nix::BuildResult::Status::MiscFailure,
                        nix::BuildResult::Status::DependencyFailed,
                        nix::BuildResult::Status::LogLimitExceeded,
                        nix::BuildResult::Status::NotDeterministic);
  }
};
}  // namespace rc

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

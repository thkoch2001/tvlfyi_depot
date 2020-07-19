#pragma once

#include "libproto/worker.grpc.pb.h"
#include "libproto/worker.pb.h"
#include "libstore/derivations.hh"
#include "libstore/store-api.hh"

namespace nix::daemon {

using ::grpc::Status;
using ::nix::proto::BuildStatus;
using ::nix::proto::PathInfo;
using ::nix::proto::StorePath;
using ::nix::proto::StorePaths;
using ::nix::proto::WorkerService;

class WorkerServiceImpl final : public WorkerService::Service {
 public:
  explicit WorkerServiceImpl(nix::Store* store) : store_(store){};

  Status IsValidPath(grpc::ServerContext* context, const StorePath* request,
                     nix::proto::IsValidPathResponse* response) override;

  Status HasSubstitutes(grpc::ServerContext* context, const StorePath* request,
                        nix::proto::HasSubstitutesResponse* response) override;

  Status QueryReferrers(grpc::ServerContext* context, const StorePath* request,
                        StorePaths* response) override;

  Status QueryValidDerivers(grpc::ServerContext* context,
                            const StorePath* request,
                            StorePaths* response) override;

  Status QueryDerivationOutputs(grpc::ServerContext* context,
                                const StorePath* request,
                                StorePaths* response) override;

  Status QueryAllValidPaths(grpc::ServerContext* context,
                            const google::protobuf::Empty* request,
                            StorePaths* response) override;

  Status QueryPathInfo(grpc::ServerContext* context, const StorePath* request,
                       PathInfo* response) override;

  Status QueryDerivationOutputNames(
      grpc::ServerContext* context, const StorePath* request,
      nix::proto::DerivationOutputNames* response) override;

  Status QueryPathFromHashPart(grpc::ServerContext* context,
                               const nix::proto::HashPart* request,
                               StorePath* response) override;

  Status QueryValidPaths(grpc::ServerContext* context,
                         const StorePaths* request,
                         StorePaths* response) override;

  Status QuerySubstitutablePaths(grpc::ServerContext* context,
                                 const StorePaths* request,
                                 StorePaths* response) override;

  Status OptimiseStore(grpc::ServerContext* context,
                       const google::protobuf::Empty* request,
                       google::protobuf::Empty* response) override;

  Status VerifyStore(grpc::ServerContext* context,
                     const nix::proto::VerifyStoreRequest* request,
                     nix::proto::VerifyStoreResponse* response) override;

  Status BuildDerivation(
      grpc::ServerContext* context,
      const nix::proto::BuildDerivationRequest* request,
      nix::proto::BuildDerivationResponse* response) override ;

  Status AddSignatures(grpc::ServerContext* context,
                       const nix::proto::AddSignaturesRequest* request,
                       google::protobuf::Empty* response) override;

  Status QueryMissing(grpc::ServerContext* context, const StorePaths* request,
                      nix::proto::QueryMissingResponse* response) override;

 private:
  nix::Store* store_;
};

}  // namespace nix::daemon

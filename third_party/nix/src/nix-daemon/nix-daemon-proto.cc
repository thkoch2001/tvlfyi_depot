#include "nix-daemon-proto.hh"

#include <google/protobuf/empty.pb.h>
#include <google/protobuf/util/time_util.h>
#include <grpcpp/impl/codegen/server_context.h>
#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/impl/codegen/status_code_enum.h>

#include "libmain/shared.hh"
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

static Status INVALID_STORE_PATH =
    Status(grpc::StatusCode::INVALID_ARGUMENT, "Invalid store path");

class WorkerServiceImpl final : public WorkerService::Service {
 public:
  WorkerServiceImpl(nix::Store& store) : store_(&store) {}

  Status IsValidPath(grpc::ServerContext* context, const StorePath* request,
                     nix::proto::IsValidPathResponse* response) override {
    const auto& path = request->path();
    store_->assertStorePath(path);
    response->set_is_valid(store_->isValidPath(path));

    return Status::OK;
  }

  Status HasSubstitutes(grpc::ServerContext* context, const StorePath* request,
                        nix::proto::HasSubstitutesResponse* response) override {
    const auto& path = request->path();
    store_->assertStorePath(path);
    PathSet res = store_->querySubstitutablePaths({path});
    response->set_has_substitutes(res.find(path) != res.end());

    return Status::OK;
  }

  Status QueryReferrers(grpc::ServerContext* context, const StorePath* request,
                        StorePaths* response) override {
    const auto& path = request->path();
    store_->assertStorePath(path);

    PathSet paths;
    store_->queryReferrers(path, paths);

    for (const auto& path : paths) {
      response->add_paths(path);
    }

    return Status::OK;
  }

  Status QueryValidDerivers(grpc::ServerContext* context,
                            const StorePath* request,
                            StorePaths* response) override {
    const auto& path = request->path();
    store_->assertStorePath(path);

    PathSet paths = store_->queryValidDerivers(path);

    for (const auto& path : paths) {
      response->add_paths(path);
    }

    return Status::OK;
  }

  Status QueryDerivationOutputs(grpc::ServerContext* context,
                                const StorePath* request,
                                StorePaths* response) override {
    const auto& path = request->path();
    store_->assertStorePath(path);

    PathSet paths = store_->queryDerivationOutputs(path);

    for (const auto& path : paths) {
      response->add_paths(path);
    }

    return Status::OK;
  }

  Status QueryAllValidPaths(grpc::ServerContext* context,
                            const google::protobuf::Empty* request,
                            StorePaths* response) override {
    const auto paths = store_->queryAllValidPaths();
    for (const auto& path : paths) {
      store_->assertStorePath(path);
      response->add_paths(path);
    }

    return Status::OK;
  }

  Status QueryPathInfo(grpc::ServerContext* context, const StorePath* request,
                       PathInfo* response) override {
    auto path = request->path();
    store_->assertStorePath(path);
    try {
      auto info = store_->queryPathInfo(path);
      response->mutable_deriver()->set_path(info->deriver);
      response->set_nar_hash(
          reinterpret_cast<const char*>(&info->narHash.hash[0]),
          info->narHash.hashSize);

      for (const auto& reference : info->references) {
        response->add_references(reference);
      }

      *response->mutable_registration_time() =
          google::protobuf::util::TimeUtil::TimeTToTimestamp(
              info->registrationTime);

      response->set_nar_size(info->narSize);
      response->set_ultimate(info->ultimate);

      for (const auto& sig : info->sigs) {
        response->add_sigs(sig);
      }

      response->set_ca(info->ca);

      return Status::OK;
    } catch (InvalidPath&) {
      return Status(grpc::StatusCode::INVALID_ARGUMENT, "Invalid store path");
    }
  }

  Status QueryDerivationOutputNames(
      grpc::ServerContext* context, const StorePath* request,
      nix::proto::DerivationOutputNames* response) override {
    auto path = request->path();
    store_->assertStorePath(path);
    auto names = store_->queryDerivationOutputNames(path);
    for (const auto& name : names) {
      response->add_names(name);
    }

    return Status::OK;
  }

  Status QueryPathFromHashPart(grpc::ServerContext* context,
                               const nix::proto::HashPart* request,
                               StorePath* response) override {
    auto hash_part = request->hash_part();
    auto path = store_->queryPathFromHashPart(hash_part);
    store_->assertStorePath(path);
    response->set_path(path);
    return Status::OK;
  }

  Status QueryValidPaths(grpc::ServerContext* context,
                         const StorePaths* request,
                         StorePaths* response) override {
    std::set<Path> paths;
    for (const auto& path : request->paths()) {
      store_->assertStorePath(path);
      paths.insert(path);
    }

    auto res = store_->queryValidPaths(paths);

    for (const auto& path : res) {
      response->add_paths(path);
    }

    return Status::OK;
  }

  Status QuerySubstitutablePaths(grpc::ServerContext* context,
                                 const StorePaths* request,
                                 StorePaths* response) override {
    std::set<Path> paths;
    for (const auto& path : request->paths()) {
      store_->assertStorePath(path);
      paths.insert(path);
    }

    auto res = store_->querySubstitutablePaths(paths);

    for (const auto& path : res) {
      response->add_paths(path);
    }

    return Status::OK;
  }

  Status OptimiseStore(grpc::ServerContext* context,
                       const google::protobuf::Empty* request,
                       google::protobuf::Empty* response) override {
    store_->optimiseStore();
    return Status::OK;
  }

  Status VerifyStore(grpc::ServerContext* context,
                     const nix::proto::VerifyStoreRequest* request,
                     nix::proto::VerifyStoreResponse* response) override {
    auto errors = store_->verifyStore(
        request->check_contents(), static_cast<RepairFlag>(request->repair()));

    response->set_errors(errors);

    return Status::OK;
  }

  Status BuildDerivation(
      grpc::ServerContext* context,
      const nix::proto::BuildDerivationRequest* request,
      nix::proto::BuildDerivationResponse* response) override {
    auto drv_path = request->drv_path().path();
    store_->assertStorePath(drv_path);
    auto drv = BasicDerivation::from_proto(&request->derivation(), *store_);

    auto build_mode = nix::build_mode_from(request->build_mode());
    if (!build_mode) {
      return Status(grpc::StatusCode::INTERNAL, "Invalid build mode");
    }

    auto res = store_->buildDerivation(drv_path, drv, *build_mode);

    response->set_status(res.status_to_proto());
    response->set_error_message(res.errorMsg);

    return Status::OK;
  }

  Status AddSignatures(grpc::ServerContext* context,
                       const nix::proto::AddSignaturesRequest* request,
                       google::protobuf::Empty* response) override {
    auto path = request->path().path();
    store_->assertStorePath(path);

    StringSet sigs;
    sigs.insert(request->sigs().sigs().begin(), request->sigs().sigs().end());

    store_->addSignatures(path, sigs);

    return Status::OK;
  }

  Status QueryMissing(grpc::ServerContext* context, const StorePaths* request,
                      nix::proto::QueryMissingResponse* response) override {
    std::set<Path> targets;
    for (auto& path : request->paths()) {
      store_->assertStorePath(path);
      targets.insert(path);
    }
    PathSet will_build;
    PathSet will_substitute;
    PathSet unknown;
    // TODO(grfn): Switch to concrete size type
    unsigned long long download_size;
    unsigned long long nar_size;

    store_->queryMissing(targets, will_build, will_substitute, unknown,
                         download_size, nar_size);
    for (auto& path : will_build) {
      response->add_will_build(path);
    }
    for (auto& path : will_substitute) {
      response->add_will_substitute(path);
    }
    for (auto& path : unknown) {
      response->add_unknown(path);
    }
    response->set_download_size(download_size);
    response->set_nar_size(nar_size);

    return Status::OK;
  };

 private:
  ref<nix::Store> store_;
};

WorkerService::Service* NewWorkerService(nix::Store& store) {
  return new WorkerServiceImpl(store);
}

}  // namespace nix::daemon

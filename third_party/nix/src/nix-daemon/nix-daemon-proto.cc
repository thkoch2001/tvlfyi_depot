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

Status WorkerServiceImpl::IsValidPath(
    grpc::ServerContext* context, const StorePath* request,
    nix::proto::IsValidPathResponse* response) {
  const auto& path = request->path();
  store_->assertStorePath(path);
  response->set_is_valid(store_->isValidPath(path));

  return Status::OK;
}

Status WorkerServiceImpl::HasSubstitutes(
    grpc::ServerContext* context, const StorePath* request,
    nix::proto::HasSubstitutesResponse* response) {
  const auto& path = request->path();
  store_->assertStorePath(path);
  PathSet res = store_->querySubstitutablePaths({path});
  response->set_has_substitutes(res.find(path) != res.end());

  return Status::OK;
}

Status WorkerServiceImpl::QueryReferrers(grpc::ServerContext* context,
                                         const StorePath* request,
                                         StorePaths* response)  {
  const auto& path = request->path();
  store_->assertStorePath(path);

  PathSet paths;
  store_->queryReferrers(path, paths);

  for (const auto& path : paths) {
    response->add_paths(path);
  }

  return Status::OK;
}

Status WorkerServiceImpl::QueryValidDerivers(grpc::ServerContext* context,
                                             const StorePath* request,
                                             StorePaths* response)  {
  const auto& path = request->path();
  store_->assertStorePath(path);

  PathSet paths = store_->queryValidDerivers(path);

  for (const auto& path : paths) {
    response->add_paths(path);
  }

  return Status::OK;
}

Status WorkerServiceImpl::QueryDerivationOutputs(
    grpc::ServerContext* context, const StorePath* request,
    StorePaths* response)  {
  const auto& path = request->path();
  store_->assertStorePath(path);

  PathSet paths = store_->queryDerivationOutputs(path);

  for (const auto& path : paths) {
    response->add_paths(path);
  }

  return Status::OK;
}

Status WorkerServiceImpl::QueryAllValidPaths(
    grpc::ServerContext* context, const google::protobuf::Empty* request,
    StorePaths* response)  {
  const auto paths = store_->queryAllValidPaths();
  for (const auto& path : paths) {
    store_->assertStorePath(path);
    response->add_paths(path);
  }

  return Status::OK;
}

Status WorkerServiceImpl::QueryPathInfo(grpc::ServerContext* context,
                                        const StorePath* request,
                                        PathInfo* response)  {
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

Status WorkerServiceImpl::QueryDerivationOutputNames(
    grpc::ServerContext* context, const StorePath* request,
    nix::proto::DerivationOutputNames* response)  {
  auto path = request->path();
  store_->assertStorePath(path);
  auto names = store_->queryDerivationOutputNames(path);
  for (const auto& name : names) {
    response->add_names(name);
  }

  return Status::OK;
}

Status WorkerServiceImpl::QueryPathFromHashPart(
    grpc::ServerContext* context, const nix::proto::HashPart* request,
    StorePath* response)  {
  auto hash_part = request->hash_part();
  auto path = store_->queryPathFromHashPart(hash_part);
  store_->assertStorePath(path);
  response->set_path(path);
  return Status::OK;
}

Status WorkerServiceImpl::QueryValidPaths(grpc::ServerContext* context,
                                          const StorePaths* request,
                                          StorePaths* response)  {
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

Status WorkerServiceImpl::QuerySubstitutablePaths(
    grpc::ServerContext* context, const StorePaths* request,
    StorePaths* response)  {
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

Status WorkerServiceImpl::OptimiseStore(
    grpc::ServerContext* context, const google::protobuf::Empty* request,
    google::protobuf::Empty* response)  {
  store_->optimiseStore();
  return Status::OK;
}

Status WorkerServiceImpl::VerifyStore(
    grpc::ServerContext* context, const nix::proto::VerifyStoreRequest* request,
    nix::proto::VerifyStoreResponse* response)  {
  auto errors = store_->verifyStore(request->check_contents(),
                                    static_cast<RepairFlag>(request->repair()));

  response->set_errors(errors);

  return Status::OK;
}

Status WorkerServiceImpl::BuildDerivation(
    grpc::ServerContext* context,
    const nix::proto::BuildDerivationRequest* request,
    nix::proto::BuildDerivationResponse* response)  {
  auto drv_path = request->drv_path().path();
  store_->assertStorePath(drv_path);
  auto drv = BasicDerivation::from_proto(&request->derivation(), store_);

  auto build_mode = nix::build_mode_from(request->build_mode());
  if (!build_mode) {
    return Status(grpc::StatusCode::INTERNAL, "Invalid build mode");
  }

  auto res = store_->buildDerivation(drv_path, drv, *build_mode);

  response->set_status(res.status_to_proto());
  response->set_error_message(res.errorMsg);

  return Status::OK;
}

Status WorkerServiceImpl::AddSignatures(
    grpc::ServerContext* context,
    const nix::proto::AddSignaturesRequest* request,
    google::protobuf::Empty* response)  {
  auto path = request->path().path();
  store_->assertStorePath(path);

  StringSet sigs;
  sigs.insert(request->sigs().sigs().begin(), request->sigs().sigs().end());

  store_->addSignatures(path, sigs);

  return Status::OK;
}

Status WorkerServiceImpl::QueryMissing(
    grpc::ServerContext* context, const StorePaths* request,
    nix::proto::QueryMissingResponse* response)  {
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
}

}  // namespace nix::daemon

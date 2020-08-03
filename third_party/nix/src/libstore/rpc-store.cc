#include "rpc-store.hh"

#include <algorithm>
#include <memory>

#include <absl/strings/str_cat.h>
#include <absl/strings/str_format.h>
#include <google/protobuf/empty.pb.h>
#include <google/protobuf/util/time_util.h>
#include <grpcpp/create_channel.h>
#include <grpcpp/impl/codegen/async_unary_call.h>
#include <grpcpp/impl/codegen/client_context.h>
#include <grpcpp/impl/codegen/completion_queue.h>
#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/impl/codegen/sync_stream.h>
#include <grpcpp/security/credentials.h>

#include "libproto/worker.grpc.pb.h"
#include "libproto/worker.pb.h"
#include "libstore/store-api.hh"
#include "libutil/hash.hh"
#include "libutil/types.hh"

namespace nix {

namespace store {

using google::protobuf::util::TimeUtil;
using grpc::ClientContext;
using nix::proto::WorkerService;

static google::protobuf::Empty kEmpty;

proto::StorePath StorePath(const Path& path) {
  proto::StorePath store_path;
  store_path.set_path(path);
  return store_path;
}

proto::StorePaths StorePaths(const PathSet& paths) {
  proto::StorePaths result;
  for (const auto& path : paths) {
    result.add_paths(path);
  }
  return result;
}

template <typename T, typename U>
T FillFrom(const U& src) {
  T result;
  result.insert(src.begin(), src.end());
  return result;
}

// TODO(grfn): Obviously this should go away and be replaced by StatusOr... but
// that would require refactoring the entire store api, which we don't feel like
// doing right now. We should at some point though
void const RpcStore::SuccessOrThrow(const grpc::Status& status) const {
  if (!status.ok()) {
    switch (status.error_code()) {
      case grpc::StatusCode::UNIMPLEMENTED:
        throw Unsupported(absl::StrFormat(
            "operation is not supported by store at %s: %s",
            uri_.value_or("unknown URI"), status.error_message()));
      default:
        throw Error(absl::StrFormat(
            "Rpc call to %s failed (%d): %s ", uri_.value_or("unknown URI"),
            status.error_code(), status.error_message()));
    }
  }
}

bool RpcStore::isValidPathUncached(const Path& path) {
  ClientContext ctx;
  proto::IsValidPathResponse resp;
  SuccessOrThrow(stub_->IsValidPath(&ctx, StorePath(path), &resp));
  return resp.is_valid();
}

PathSet RpcStore::queryAllValidPaths() {
  ClientContext ctx;
  proto::StorePaths paths;
  SuccessOrThrow(stub_->QueryAllValidPaths(&ctx, kEmpty, &paths));
  return FillFrom<PathSet>(paths.paths());
}

PathSet RpcStore::queryValidPaths(const PathSet& paths,
                                  SubstituteFlag maybeSubstitute) {
  ClientContext ctx;
  proto::StorePaths store_paths;
  for (const auto& path : paths) {
    store_paths.add_paths(path);
  }
  proto::StorePaths result_paths;
  SuccessOrThrow(stub_->QueryValidPaths(&ctx, store_paths, &result_paths));
  return FillFrom<PathSet>(result_paths.paths());
}

void RpcStore::queryPathInfoUncached(
    const Path& path,
    Callback<std::shared_ptr<ValidPathInfo>> callback) noexcept {
  ClientContext ctx;
  proto::StorePath store_path;
  store_path.set_path(path);

  try {
    proto::PathInfo path_info;
    SuccessOrThrow(stub_->QueryPathInfo(&ctx, store_path, &path_info));

    std::shared_ptr<ValidPathInfo> info;

    if (!path_info.is_valid()) {
      throw InvalidPath(absl::StrFormat("path '%s' is not valid", path));
    }

    info = std::make_shared<ValidPathInfo>();
    info->path = path;
    info->deriver = path_info.deriver().path();
    if (!info->deriver.empty()) {
      assertStorePath(info->deriver);
    }
    auto hash_ = Hash::deserialize(path_info.nar_hash(), htSHA256);
    info->narHash = Hash::unwrap_throw(hash_);
    info->references.insert(path_info.references().begin(),
                            path_info.references().end());
    info->registrationTime =
        TimeUtil::TimestampToTimeT(path_info.registration_time());
    info->narSize = path_info.nar_size();
    info->ultimate = path_info.ultimate();
    info->sigs.insert(path_info.sigs().begin(), path_info.sigs().end());
    info->ca = path_info.ca();

    callback(std::move(info));
  } catch (...) {
    callback.rethrow();
  }
}

void RpcStore::queryReferrers(const Path& path, PathSet& referrers) {
  ClientContext ctx;
  proto::StorePaths paths;
  SuccessOrThrow(stub_->QueryReferrers(&ctx, StorePath(path), &paths));
  referrers.insert(paths.paths().begin(), paths.paths().end());
}

PathSet RpcStore::queryValidDerivers(const Path& path) {
  ClientContext ctx;
  proto::StorePaths paths;
  SuccessOrThrow(stub_->QueryValidDerivers(&ctx, StorePath(path), &paths));
  return FillFrom<PathSet>(paths.paths());
}

PathSet RpcStore::queryDerivationOutputs(const Path& path) {
  ClientContext ctx;
  proto::StorePaths paths;
  SuccessOrThrow(stub_->QueryDerivationOutputs(&ctx, StorePath(path), &paths));
  return FillFrom<PathSet>(paths.paths());
}

StringSet RpcStore::queryDerivationOutputNames(const Path& path) {
  ClientContext ctx;
  proto::DerivationOutputNames output_names;
  SuccessOrThrow(
      stub_->QueryDerivationOutputNames(&ctx, StorePath(path), &output_names));
  return FillFrom<StringSet>(output_names.names());
}

Path RpcStore::queryPathFromHashPart(const std::string& hashPart) {
  ClientContext ctx;
  proto::StorePath path;
  proto::HashPart proto_hash_part;
  proto_hash_part.set_hash_part(hashPart);
  SuccessOrThrow(stub_->QueryPathFromHashPart(&ctx, proto_hash_part, &path));
  return path.path();
}

PathSet RpcStore::querySubstitutablePaths(const PathSet& paths) {
  ClientContext ctx;
  proto::StorePaths result;
  SuccessOrThrow(
      stub_->QuerySubstitutablePaths(&ctx, StorePaths(paths), &result));
  return FillFrom<PathSet>(result.paths());
}

void RpcStore::querySubstitutablePathInfos(const PathSet& paths,
                                           SubstitutablePathInfos& infos) {
  ClientContext ctx;
  proto::SubstitutablePathInfos result;
  SuccessOrThrow(
      stub_->QuerySubstitutablePathInfos(&ctx, StorePaths(paths), &result));

  for (const auto& path_info : result.path_infos()) {
    auto path = path_info.path().path();
    SubstitutablePathInfo& info(infos[path]);
    info.deriver = path_info.deriver().path();
    if (!info.deriver.empty()) {
      assertStorePath(info.deriver);
    }
    info.references = FillFrom<PathSet>(path_info.references());
    info.downloadSize = path_info.download_size();
    info.narSize = path_info.nar_size();
  }
}

void RpcStore::addToStore(const ValidPathInfo& info, Source& narSource,
                          RepairFlag repair, CheckSigsFlag checkSigs,
                          std::shared_ptr<FSAccessor> accessor) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::addToStore(const ValidPathInfo& info,
                          const ref<std::string>& nar, RepairFlag repair,
                          CheckSigsFlag checkSigs,
                          std::shared_ptr<FSAccessor> accessor) {
  throw absl::StrCat("Not implemented ", __func__);
}

Path RpcStore::addToStore(const std::string& name, const Path& srcPath,
                          bool recursive, HashType hashAlgo, PathFilter& filter,
                          RepairFlag repair) {
  throw absl::StrCat("Not implemented ", __func__);
}

Path RpcStore::addTextToStore(const std::string& name, const std::string& s,
                              const PathSet& references, RepairFlag repair) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::narFromPath(const Path& path, Sink& sink) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::buildPaths(const PathSet& paths, BuildMode buildMode) {
  throw absl::StrCat("Not implemented ", __func__);
}

BuildResult RpcStore::buildDerivation(const Path& drvPath,
                                      const BasicDerivation& drv,
                                      BuildMode buildMode) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::ensurePath(const Path& path) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::addTempRoot(const Path& path) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::addIndirectRoot(const Path& path) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::syncWithGC() {
  throw absl::StrCat("Not implemented ", __func__);
}

Roots RpcStore::findRoots(bool censor) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::collectGarbage(const GCOptions& options, GCResults& results) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::optimiseStore() {
  throw absl::StrCat("Not implemented ", __func__);
}

bool RpcStore::verifyStore(bool checkContents, RepairFlag repair) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::addSignatures(const Path& storePath, const StringSet& sigs) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::computeFSClosure(const PathSet& paths, PathSet& paths_,
                                bool flipDirection, bool includeOutputs,
                                bool includeDerivers) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::queryMissing(const PathSet& targets, PathSet& willBuild,
                            PathSet& willSubstitute, PathSet& unknown,
                            unsigned long long& downloadSize,
                            unsigned long long& narSize) {
  throw absl::StrCat("Not implemented ", __func__);
}

std::shared_ptr<std::string> RpcStore::getBuildLog(const Path& path) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::connect() { throw absl::StrCat("Not implemented ", __func__); }

unsigned int RpcStore::getProtocol() {
  throw absl::StrCat("Not implemented ", __func__);
}

int RpcStore::getPriority() {
  throw absl::StrCat("Not implemented ", __func__);
}

Path RpcStore::toRealPath(const Path& storePath) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::createUser(const std::string& userName, uid_t userId) {
  throw absl::StrCat("Not implemented ", __func__);
}

}  // namespace store

static std::string uriScheme = "unix://";

// TODO(grfn): Make this a function that we call from main rather than... this
static RegisterStoreImplementation regStore([](const std::string& uri,
                                               const Store::Params& params)
                                                -> std::shared_ptr<Store> {
  if (std::string(uri, 0, uriScheme.size()) != uriScheme) {
    return nullptr;
  }
  auto channel = grpc::CreateChannel(uri, grpc::InsecureChannelCredentials());
  return std::make_shared<store::RpcStore>(
      uri, params, proto::WorkerService::NewStub(channel));
});

}  // namespace nix

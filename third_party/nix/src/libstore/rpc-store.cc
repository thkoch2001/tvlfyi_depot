#include "rpc-store.hh"

#include <algorithm>
#include <filesystem>
#include <memory>

#include <absl/status/status.h>
#include <absl/strings/str_cat.h>
#include <absl/strings/str_format.h>
#include <absl/strings/string_view.h>
#include <glog/logging.h>
#include <google/protobuf/empty.pb.h>
#include <google/protobuf/util/time_util.h>
#include <grpcpp/create_channel.h>
#include <grpcpp/impl/codegen/async_unary_call.h>
#include <grpcpp/impl/codegen/client_context.h>
#include <grpcpp/impl/codegen/completion_queue.h>
#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/impl/codegen/sync_stream.h>
#include <grpcpp/security/credentials.h>
#include <sys/ucontext.h>

#include "libproto/worker.grpc.pb.h"
#include "libproto/worker.pb.h"
#include "libstore/store-api.hh"
#include "libutil/archive.hh"
#include "libutil/hash.hh"
#include "libutil/proto.hh"
#include "libutil/types.hh"

namespace nix {

namespace store {

using google::protobuf::util::TimeUtil;
using grpc::ClientContext;
using nix::proto::WorkerService;

static google::protobuf::Empty kEmpty;

template <typename Request>
class RPCSink : public BufferedSink {
 public:
  using Writer = grpc::ClientWriter<Request>;
  explicit RPCSink(std::unique_ptr<Writer>&& writer)
      : writer_(std::move(writer)), good_(true) {}

  bool good() override { return good_; }

  void write(const unsigned char* data, size_t len) override {
    Request req;
    req.set_data(data, len);
    if (!writer_->Write(req)) {
      good_ = false;
    }
  }

  ~RPCSink() override { flush(); }

  grpc::Status Finish() {
    flush();
    return writer_->Finish();
  }

 private:
  std::unique_ptr<Writer> writer_;
  bool good_;
};

// TODO(grfn): Obviously this should go away and be replaced by StatusOr... but
// that would require refactoring the entire store api, which we don't feel like
// doing right now. We should at some point though
void const RpcStore::SuccessOrThrow(const grpc::Status& status,
                                    const absl::string_view& call) const {
  if (!status.ok()) {
    auto uri = uri_.value_or("unknown URI");
    switch (status.error_code()) {
      case grpc::StatusCode::UNIMPLEMENTED:
        throw Unsupported(
            absl::StrFormat("operation %s is not supported by store at %s: %s",
                            call, uri, status.error_message()));
      default:
        throw Error(absl::StrFormat(
            "Rpc call %s to %s failed (%s): %s ", call, uri,
            util::proto::GRPCStatusCodeDescription(status.error_code()),
            status.error_message()));
    }
  }
}

bool RpcStore::isValidPathUncached(const Path& path) {
  ClientContext ctx;
  proto::IsValidPathResponse resp;
  SuccessOrThrow(stub_->IsValidPath(&ctx, util::proto::StorePath(path), &resp),
                 __FUNCTION__);
  return resp.is_valid();
}

PathSet RpcStore::queryAllValidPaths() {
  ClientContext ctx;
  proto::StorePaths paths;
  SuccessOrThrow(stub_->QueryAllValidPaths(&ctx, kEmpty, &paths), __FUNCTION__);
  return util::proto::FillFrom<PathSet>(paths.paths());
}

PathSet RpcStore::queryValidPaths(const PathSet& paths,
                                  SubstituteFlag maybeSubstitute) {
  ClientContext ctx;
  proto::StorePaths store_paths;
  for (const auto& path : paths) {
    store_paths.add_paths(path);
  }
  proto::StorePaths result_paths;
  SuccessOrThrow(stub_->QueryValidPaths(&ctx, store_paths, &result_paths),
                 __FUNCTION__);
  return util::proto::FillFrom<PathSet>(result_paths.paths());
}

void RpcStore::queryPathInfoUncached(
    const Path& path,
    Callback<std::shared_ptr<ValidPathInfo>> callback) noexcept {
  ClientContext ctx;
  proto::StorePath store_path;
  store_path.set_path(path);

  try {
    proto::PathInfo path_info;
    SuccessOrThrow(stub_->QueryPathInfo(&ctx, store_path, &path_info),
                   __FUNCTION__);

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
  SuccessOrThrow(
      stub_->QueryReferrers(&ctx, util::proto::StorePath(path), &paths),
      __FUNCTION__);
  referrers.insert(paths.paths().begin(), paths.paths().end());
}

PathSet RpcStore::queryValidDerivers(const Path& path) {
  ClientContext ctx;
  proto::StorePaths paths;
  SuccessOrThrow(
      stub_->QueryValidDerivers(&ctx, util::proto::StorePath(path), &paths),
      __FUNCTION__);
  return util::proto::FillFrom<PathSet>(paths.paths());
}

PathSet RpcStore::queryDerivationOutputs(const Path& path) {
  ClientContext ctx;
  proto::StorePaths paths;
  SuccessOrThrow(
      stub_->QueryDerivationOutputs(&ctx, util::proto::StorePath(path), &paths),
      __FUNCTION__);
  return util::proto::FillFrom<PathSet>(paths.paths());
}

StringSet RpcStore::queryDerivationOutputNames(const Path& path) {
  ClientContext ctx;
  proto::DerivationOutputNames output_names;
  SuccessOrThrow(stub_->QueryDerivationOutputNames(
      &ctx, util::proto::StorePath(path), &output_names));
  return util::proto::FillFrom<StringSet>(output_names.names());
}

Path RpcStore::queryPathFromHashPart(const std::string& hashPart) {
  ClientContext ctx;
  proto::StorePath path;
  proto::HashPart proto_hash_part;
  proto_hash_part.set_hash_part(hashPart);
  SuccessOrThrow(stub_->QueryPathFromHashPart(&ctx, proto_hash_part, &path),
                 __FUNCTION__);
  return path.path();
}

PathSet RpcStore::querySubstitutablePaths(const PathSet& paths) {
  ClientContext ctx;
  proto::StorePaths result;
  SuccessOrThrow(stub_->QuerySubstitutablePaths(
      &ctx, util::proto::StorePaths(paths), &result));
  return util::proto::FillFrom<PathSet>(result.paths());
}

void RpcStore::querySubstitutablePathInfos(const PathSet& paths,
                                           SubstitutablePathInfos& infos) {
  ClientContext ctx;
  proto::SubstitutablePathInfos result;
  SuccessOrThrow(stub_->QuerySubstitutablePathInfos(
      &ctx, util::proto::StorePaths(paths), &result));

  for (const auto& path_info : result.path_infos()) {
    auto path = path_info.path().path();
    SubstitutablePathInfo& info(infos[path]);
    info.deriver = path_info.deriver().path();
    if (!info.deriver.empty()) {
      assertStorePath(info.deriver);
    }
    info.references = util::proto::FillFrom<PathSet>(path_info.references());
    info.downloadSize = path_info.download_size();
    info.narSize = path_info.nar_size();
  }
}

void RpcStore::addToStore(const ValidPathInfo& info, Source& narSource,
                          RepairFlag repair, CheckSigsFlag checkSigs,
                          std::shared_ptr<FSAccessor> accessor) {
  ClientContext ctx;
  google::protobuf::Empty response;
  auto writer = stub_->AddToStoreNar(&ctx, &response);

  proto::AddToStoreNarRequest path_info_req;
  path_info_req.mutable_path_info()->mutable_path()->set_path(info.path);
  path_info_req.mutable_path_info()->mutable_deriver()->set_path(info.deriver);
  path_info_req.mutable_path_info()->set_nar_hash(
      info.narHash.to_string(Base16, false));
  for (const auto& ref : info.references) {
    path_info_req.mutable_path_info()->add_references(ref);
  }
  *path_info_req.mutable_path_info()->mutable_registration_time() =
      TimeUtil::TimeTToTimestamp(info.registrationTime);
  path_info_req.mutable_path_info()->set_nar_size(info.narSize);
  path_info_req.mutable_path_info()->set_ultimate(info.ultimate);
  for (const auto& sig : info.sigs) {
    path_info_req.mutable_path_info()->add_sigs(sig);
  }
  path_info_req.mutable_path_info()->set_ca(info.ca);
  path_info_req.mutable_path_info()->set_repair(repair);
  path_info_req.mutable_path_info()->set_check_sigs(checkSigs);

  if (!writer->Write(path_info_req)) {
    throw Error("Could not write to nix daemon");
  }

  RPCSink sink(std::move(writer));
  copyNAR(narSource, sink);
  SuccessOrThrow(sink.Finish(), __FUNCTION__);
}

Path RpcStore::addToStore(const std::string& name, const Path& srcPath,
                          bool recursive, HashType hashAlgo, PathFilter& filter,
                          RepairFlag repair) {
  if (repair != 0u) {
    throw Error(
        "repairing is not supported when building through the Nix daemon");
  }

  ClientContext ctx;
  proto::StorePath response;
  auto writer = stub_->AddToStore(&ctx, &response);

  proto::AddToStoreRequest metadata_req;
  metadata_req.mutable_meta()->set_base_name(name);
  // TODO(grfn): what is fixed?
  metadata_req.mutable_meta()->set_fixed(!(hashAlgo == htSHA256 && recursive));
  metadata_req.mutable_meta()->set_recursive(recursive);
  metadata_req.mutable_meta()->set_hash_type(HashTypeToProto(hashAlgo));

  if (!writer->Write(metadata_req)) {
    throw Error("Could not write to nix daemon");
  }

  RPCSink sink(std::move(writer));
  dumpPath(std::filesystem::absolute(srcPath), sink);
  sink.flush();
  SuccessOrThrow(sink.Finish(), __FUNCTION__);

  return response.path();
}

Path RpcStore::addTextToStore(const std::string& name,
                              const std::string& content,
                              const PathSet& references, RepairFlag repair) {
  if (repair != 0u) {
    throw Error(
        "repairing is not supported when building through the Nix daemon");
  }
  ClientContext ctx;
  proto::AddTextToStoreRequest request;
  request.set_name(name);
  request.set_content(content);
  for (const auto& ref : references) {
    request.add_references(ref);
  }
  proto::StorePath result;
  SuccessOrThrow(stub_->AddTextToStore(&ctx, request, &result), __FUNCTION__);
  return result.path();
}

void RpcStore::narFromPath(const Path& path, Sink& sink) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

void RpcStore::buildPaths(const PathSet& paths, BuildMode buildMode) {
  ClientContext ctx;
  proto::BuildPathsRequest request;
  for (const auto& path : paths) {
    request.add_drvs(path);
  }
  google::protobuf::Empty response;
  request.set_mode(nix::BuildModeToProto(buildMode));
  SuccessOrThrow(stub_->BuildPaths(&ctx, request, &response), __FUNCTION__);
}

BuildResult RpcStore::buildDerivation(const Path& drvPath,
                                      const BasicDerivation& drv,
                                      BuildMode buildMode) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

void RpcStore::ensurePath(const Path& path) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

void RpcStore::addTempRoot(const Path& path) {
  ClientContext ctx;
  google::protobuf::Empty response;
  SuccessOrThrow(
      stub_->AddTempRoot(&ctx, util::proto::StorePath(path), &response),
      __FUNCTION__);
}

void RpcStore::addIndirectRoot(const Path& path) {
  ClientContext ctx;
  google::protobuf::Empty response;
  SuccessOrThrow(
      stub_->AddIndirectRoot(&ctx, util::proto::StorePath(path), &response),
      __FUNCTION__);
}

void RpcStore::syncWithGC() {
  ClientContext ctx;
  google::protobuf::Empty response;
  SuccessOrThrow(stub_->SyncWithGC(&ctx, kEmpty, &response), __FUNCTION__);
}

Roots RpcStore::findRoots(bool censor) {
  ClientContext ctx;
  proto::FindRootsResponse response;
  SuccessOrThrow(stub_->FindRoots(&ctx, kEmpty, &response), __FUNCTION__);
  Roots result;

  for (const auto& [target, links] : response.roots()) {
    auto link_paths =
        util::proto::FillFrom<std::unordered_set<std::string>>(links.paths());
    result.insert({target, link_paths});
  }

  return result;
}

void RpcStore::collectGarbage(const GCOptions& options, GCResults& results) {
  ClientContext ctx;
  proto::CollectGarbageRequest request;
  request.set_action(options.ActionToProto());
  for (const auto& path : options.pathsToDelete) {
    request.add_paths_to_delete(path);
  }
  request.set_ignore_liveness(options.ignoreLiveness);
  request.set_max_freed(options.maxFreed);

  proto::CollectGarbageResponse response;
  SuccessOrThrow(stub_->CollectGarbage(&ctx, request, &response), __FUNCTION__);

  for (const auto& path : response.deleted_paths()) {
    results.paths.insert(path);
  }
  results.bytesFreed = response.bytes_freed();
}

void RpcStore::optimiseStore() {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

bool RpcStore::verifyStore(bool checkContents, RepairFlag repair) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

void RpcStore::addSignatures(const Path& storePath, const StringSet& sigs) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

void RpcStore::computeFSClosure(const PathSet& paths, PathSet& paths_,
                                bool flipDirection, bool includeOutputs,
                                bool includeDerivers) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

void RpcStore::queryMissing(const PathSet& targets, PathSet& willBuild,
                            PathSet& willSubstitute, PathSet& unknown,
                            unsigned long long& downloadSize,
                            unsigned long long& narSize) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

std::shared_ptr<std::string> RpcStore::getBuildLog(const Path& path) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

void RpcStore::connect() {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

unsigned int RpcStore::getProtocol() {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

int RpcStore::getPriority() {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

Path RpcStore::toRealPath(const Path& storePath) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
}

void RpcStore::createUser(const std::string& userName, uid_t userId) {
  throw Unsupported(absl::StrCat("Not implemented ", __func__));
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

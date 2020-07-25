#include "rpc_store.hh"

#include <absl/strings/str_cat.h>
#include <absl/strings/str_format.h>
#include <grpcpp/create_channel.h>
#include <grpcpp/impl/codegen/client_context.h>
#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/security/credentials.h>

#include "libproto/worker.grpc.pb.h"
#include "libproto/worker.pb.h"

namespace nix {

namespace store {

using grpc::ClientContext;
using nix::proto::WorkerService;

proto::StorePath StorePath(const Path& path) {
  proto::StorePath store_path;
  store_path.set_path(path);
  return store_path;
}

// TODO(grfn): Obviously this should go away and be replaced by StatusOr... but
// that would require refactoring the entire store api, which we don't feel like
// doing right now. We should at some point though
void SuccessOrThrow(const grpc::Status& status) {
  if (!status.ok()) {
    throw Error(absl::StrFormat("Rpc call failed (%d): %s ",
                                status.error_code(), status.error_message()));
  }
}

bool RpcStore::isValidPathUncached(const Path& path) {
  ClientContext ctx;
  proto::IsValidPathResponse resp;
  SuccessOrThrow(stub_->IsValidPath(&ctx, StorePath(path), &resp));
  return resp.is_valid();
}

PathSet RpcStore::queryAllValidPaths() {
  throw absl::StrCat("Not implemented ", __func__);
}

PathSet RpcStore::queryValidPaths(const PathSet& paths,
                                  SubstituteFlag maybeSubstitute) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::queryPathInfoUncached(
    const Path& path,
    Callback<std::shared_ptr<ValidPathInfo>> callback) noexcept {
  abort();  // implement me owo
}

void RpcStore::queryReferrers(const Path& path, PathSet& referrers) {
  throw absl::StrCat("Not implemented ", __func__);
}

PathSet RpcStore::queryValidDerivers(const Path& path) {
  throw absl::StrCat("Not implemented ", __func__);
}

PathSet RpcStore::queryDerivationOutputs(const Path& path) {
  throw absl::StrCat("Not implemented ", __func__);
}

StringSet RpcStore::queryDerivationOutputNames(const Path& path) {
  throw absl::StrCat("Not implemented ", __func__);
}

Path RpcStore::queryPathFromHashPart(const std::string& hashPart) {
  throw absl::StrCat("Not implemented ", __func__);
}

PathSet RpcStore::querySubstitutablePaths(const PathSet& paths) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::querySubstitutablePathInfos(const PathSet& paths,
                                           SubstitutablePathInfos& infos) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::addToStore(const ValidPathInfo& info, Source& narSource,
                          RepairFlag repair = NoRepair,
                          CheckSigsFlag checkSigs = CheckSigs,
                          std::shared_ptr<FSAccessor> accessor = 0) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::addToStore(const ValidPathInfo& info,
                          const ref<std::string>& nar,
                          RepairFlag repair = NoRepair,
                          CheckSigsFlag checkSigs = CheckSigs,
                          std::shared_ptr<FSAccessor> accessor = 0) {
  throw absl::StrCat("Not implemented ", __func__);
}

Path RpcStore::addToStore(const std::string& name, const Path& srcPath,
                          bool recursive = true, HashType hashAlgo = htSHA256,
                          PathFilter& filter = defaultPathFilter,
                          RepairFlag repair = NoRepair) {
  throw absl::StrCat("Not implemented ", __func__);
}

Path RpcStore::addTextToStore(const std::string& name, const std::string& s,
                              const PathSet& references,
                              RepairFlag repair = NoRepair) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::narFromPath(const Path& path, Sink& sink) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::buildPaths(const PathSet& paths,
                          BuildMode buildMode = bmNormal) {
  throw absl::StrCat("Not implemented ", __func__);
}

BuildResult RpcStore::buildDerivation(const Path& drvPath,
                                      const BasicDerivation& drv,
                                      BuildMode buildMode = bmNormal) {
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

bool RpcStore::verifyStore(bool checkContents, RepairFlag repair = NoRepair) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::addSignatures(const Path& storePath, const StringSet& sigs) {
  throw absl::StrCat("Not implemented ", __func__);
}

void RpcStore::computeFSClosure(const PathSet& paths, PathSet& paths_,
                                bool flipDirection = false,
                                bool includeOutputs = false,
                                bool includeDerivers = false) {
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

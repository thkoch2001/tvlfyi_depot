#include "nix-daemon-proto.hh"

#include <filesystem>
#include <ostream>
#include <sstream>
#include <streambuf>
#include <string>

#include <absl/container/flat_hash_set.h>
#include <absl/strings/str_cat.h>
#include <absl/strings/str_format.h>
#include <glog/logging.h>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/empty.pb.h>
#include <google/protobuf/reflection.h>
#include <google/protobuf/util/time_util.h>
#include <grpcpp/impl/codegen/server_context.h>
#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/impl/codegen/status_code_enum.h>
#include <grpcpp/impl/codegen/sync_stream.h>

#include "libmain/shared.hh"
#include "libproto/common.pb.h"
#include "libproto/worker.grpc.pb.h"
#include "libproto/worker.pb.h"
#include "libstore/derivations.hh"
#include "libstore/local-store.hh"
#include "libstore/store-api.hh"
#include "libutil/archive.hh"
#include "libutil/hash.hh"
#include "libutil/proto.hh"
#include "libutil/serialise.hh"
#include "libutil/types.hh"

namespace nix::daemon {

using ::google::protobuf::util::TimeUtil;
using ::grpc::Status;
using ::nix::proto::PathInfo;
using ::nix::proto::StorePath;
using ::nix::proto::StorePaths;
using ::nix::proto::WorkerService;

template <typename Request>
class RPCSource final : public Source {
 public:
  using Reader = grpc::ServerReader<Request>;
  explicit RPCSource(Reader* reader) : reader_(reader) {}

  size_t read(unsigned char* data, size_t len) override {
    auto got = buffer_.sgetn(reinterpret_cast<char*>(data), len);
    if (got < len) {
      Request msg;
      if (!reader_->Read(&msg)) {
        return got;
      }
      if (msg.add_oneof_case() != Request::kData) {
        // TODO(grfn): Make Source::read return a StatusOr and get rid of this
        // throw
        throw Error(
            "Invalid AddToStoreRequest: all messages except the first must "
            "contain data");
      }
      buffer_.sputn(msg.data().data(), msg.data().length());
      return got + read(data + got, len - got);
    }
    return got;
  };

 private:
  std::stringbuf buffer_;
  Reader* reader_;
};

// TODO(grfn): Make this some sort of pipe so we don't have to store data in
// memory
/* If the NAR archive contains a single file at top-level, then save
   the contents of the file to `s'.  Otherwise barf. */
struct RetrieveRegularNARSink : ParseSink {
  bool regular{true};
  std::string s;

  RetrieveRegularNARSink() {}

  void createDirectory(const Path& path) override { regular = false; }

  void receiveContents(unsigned char* data, unsigned int len) override {
    s.append(reinterpret_cast<const char*>(data), len);
  }

  void createSymlink(const Path& path, const std::string& target) override {
    regular = false;
  }
};

#define ASSERT_INPUT_STORE_PATH(path)                                          \
  if (!store_->isStorePath(path)) {                                            \
    return Status(grpc::StatusCode::INVALID_ARGUMENT,                          \
                  absl::StrFormat("path '%s' is not in the Nix store", path)); \
  }

class BuildLogStreambuf final : public std::streambuf {
 public:
  using Writer = grpc::ServerWriter<nix::proto::BuildEvent>;
  explicit BuildLogStreambuf(Writer* writer) : writer_(writer) {}

  std::streamsize xsputn(const char_type* s, std::streamsize n) override {
    nix::proto::BuildEvent event;
    event.mutable_build_log()->set_line(s, n);
    writer_->Write(event);
    return n;
  }

 private:
  Writer* writer_{};
};

absl::Status ApplySettings(grpc::ServerContext* context) {
  auto metadata = context->client_metadata();
  // Need to convert string_view to grpc::string_ref
  grpc::string_ref optionsIdent = grpc::string_ref(
      kNixOptionsIdentifier.data(), kNixOptionsIdentifier.length());

  auto range = metadata.equal_range(optionsIdent);
  nix::proto::RemoteOptions options;
  for (auto it = range.first; it != range.second; ++it) {
    // DCHECK(it->first == optionsIdent);
    // TODO(riking): zero copy this?
    std::string data(it->second.cbegin(), it->second.cend());
    if (!options.ParseFromString(data)) {
      return absl::InvalidArgumentError("could not parse nix.options");
    }
  }

  auto descriptor = options.GetDescriptor();
  auto reflection = options.GetReflection();
  for (int fnum = 0; fnum < descriptor->field_count(); fnum++) {
    auto field = descriptor->field(fnum);
    if (!reflection->HasField(options, field)) {
      continue;
    }
    if (!field->options().HasExtension(nix::proto::setting)) {
      continue;
    }
    auto ext = field->options().GetExtension(nix::proto::setting);
    const auto& setting_name = ext.name();

    switch (field->number()) {
      case nix::proto::RemoteOptions::kMaxSilentTimeFieldNumber: {
        settings.set(setting_name, absl::StrCat(TimeUtil::DurationToSeconds(
                                       options.max_silent_time())));
        break;
      }
      case nix::proto::RemoteOptions::kBuildTimeoutFieldNumber: {
        settings.set(
            setting_name,
            absl::StrCat(TimeUtil::DurationToSeconds(options.build_timeout())));
        break;
      }
      case nix::proto::RemoteOptions::kConnectTimeoutFieldNumber: {
        settings.set(setting_name, absl::StrCat(TimeUtil::DurationToSeconds(
                                       options.connect_timeout())));
        break;
      }
      case nix::proto::RemoteOptions::kSubstitutersFieldNumber:
      case nix::proto::RemoteOptions::kExtraSubstitutersFieldNumber: {
        absl::flat_hash_set<absl::string_view> permittedSubstituters;
        permittedSubstituters.insert(settings.trustedSubstituters.get().begin(),
                                     settings.trustedSubstituters.get().end());
        permittedSubstituters.insert(settings.substituters.get().begin(),
                                     settings.substituters.get().end());

        auto& provided = (field->number() ==
                       nix::proto::RemoteOptions::kSubstitutersFieldNumber)
                          ? options.substituters()
                          : options.extra_substituters();
        nix::Strings result;
        for (const auto& s : provided) {
          if (permittedSubstituters.contains(s)) {
            result.push_back(s);
          } else {
            LOG(WARNING) << "ignoring untrusted substituter from client: '" << s
                         << "'";
          }
        }

        if (field->number() ==
            nix::proto::RemoteOptions::kSubstitutersFieldNumber) {
          settings.substituters = result;
        } else {
          settings.extraSubstituters = result;
        }
        break;
      }
      default:
        using CppType = google::protobuf::FieldDescriptor::CppType;
        switch (field->cpp_type()) {
          case CppType::CPPTYPE_INT64: {
            settings.set(setting_name,
                         absl::StrCat(reflection->GetInt64(options, field)));
            break;
          }
          case CppType::CPPTYPE_BOOL: {
            bool v = reflection->GetBool(options, field);
            settings.set(setting_name, v ? "true" : "false");
            break;
          }
          default: {
            return absl::UnimplementedError(
                "common setting field's type not supported");
          }
        }
        break;
    }
  }

  if (false /* user is trusted? */) {
    for (const auto& pair : options.overrides()) {
      settings.set(pair.first, pair.second);
    }
  }
}

class WorkerServiceImpl final : public WorkerService::Service {
 public:
  WorkerServiceImpl(nix::Store& store) : store_(&store) {}

  Status IsValidPath(grpc::ServerContext* context, const StorePath* request,
                     nix::proto::IsValidPathResponse* response) override {
    return HandleExceptions(
        [&]() -> Status {
          const auto& path = request->path();
          response->set_is_valid(store_->isValidPath(path));

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status HasSubstitutes(grpc::ServerContext* context, const StorePath* request,
                        nix::proto::HasSubstitutesResponse* response) override {
    return HandleExceptions(
        [&]() -> Status {
          const auto& path = request->path();
          ASSERT_INPUT_STORE_PATH(path);
          PathSet res = store_->querySubstitutablePaths({path});
          response->set_has_substitutes(res.find(path) != res.end());

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QueryReferrers(grpc::ServerContext* context, const StorePath* request,
                        StorePaths* response) override {
    return HandleExceptions(
        [&]() -> Status {
          const auto& path = request->path();
          ASSERT_INPUT_STORE_PATH(path);

          PathSet paths;
          store_->queryReferrers(path, paths);

          for (const auto& path : paths) {
            response->add_paths(path);
          }

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status AddToStore(grpc::ServerContext* context,
                    grpc::ServerReader<nix::proto::AddToStoreRequest>* reader,
                    nix::proto::StorePath* response) override {
    return HandleExceptions(
        [&]() -> Status {
          proto::AddToStoreRequest metadata_request;
          auto has_metadata = reader->Read(&metadata_request);

          if (!has_metadata || !metadata_request.has_meta()) {
            return Status(grpc::StatusCode::INVALID_ARGUMENT,
                          "Metadata must be set before sending file content");
          }

          auto meta = metadata_request.meta();
          RPCSource source(reader);
          auto opt_hash_type = hash_type_from(meta.hash_type());
          if (!opt_hash_type) {
            return Status(grpc::StatusCode::INVALID_ARGUMENT,
                          "Invalid hash type");
          }

          std::string* data;
          RetrieveRegularNARSink nar;
          TeeSource saved_nar(source);

          if (meta.recursive()) {
            // TODO(grfn): Don't store the full data in memory, instead just
            // make addToStoreFromDump take a Source
            ParseSink sink;
            parseDump(sink, saved_nar);
            data = &(*saved_nar.data);
          } else {
            parseDump(nar, source);
            if (!nar.regular) {
              return Status(grpc::StatusCode::INVALID_ARGUMENT,
                            "Regular file expected");
            }
            data = &nar.s;
          }

          auto local_store = store_.dynamic_pointer_cast<LocalStore>();
          if (!local_store) {
            return Status(grpc::StatusCode::FAILED_PRECONDITION,
                          "operation is only supported by LocalStore");
          }

          auto path = local_store->addToStoreFromDump(
              *data, meta.base_name(), meta.recursive(), opt_hash_type.value());

          response->set_path(path);

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status AddToStoreNar(
      grpc::ServerContext* context,
      grpc::ServerReader<nix::proto::AddToStoreNarRequest>* reader,
      google::protobuf::Empty*) override {
    return HandleExceptions(
        [&]() -> Status {
          proto::AddToStoreNarRequest path_info_request;
          auto has_path_info = reader->Read(&path_info_request);
          if (!has_path_info || !path_info_request.has_path_info()) {
            return Status(grpc::StatusCode::INVALID_ARGUMENT,
                          "Path info must be set before sending nar content");
          }

          auto path_info = path_info_request.path_info();

          ValidPathInfo info;
          info.path = path_info.path().path();
          info.deriver = path_info.deriver().path();

          if (!info.deriver.empty()) {
            ASSERT_INPUT_STORE_PATH(info.deriver);
          }

          auto nar_hash = Hash::deserialize(path_info.nar_hash(), htSHA256);

          if (!nar_hash.ok()) {
            return Status(grpc::StatusCode::INVALID_ARGUMENT,
                          std::string(nar_hash.status().message()));
          }

          info.narHash = nar_hash.ConsumeValueOrDie();
          for (const auto& ref : path_info.references()) {
            info.references.insert(ref);
          }
          info.registrationTime =
              TimeUtil::TimestampToTimeT(path_info.registration_time());
          info.narSize = path_info.nar_size();
          info.ultimate = path_info.ultimate();
          for (const auto& sig : path_info.sigs()) {
            info.sigs.insert(sig);
          }
          info.ca = path_info.ca();

          auto repair = path_info.repair();
          auto check_sigs = path_info.check_sigs();

          std::string saved;
          RPCSource source(reader);
          store_->addToStore(info, source, repair ? Repair : NoRepair,
                             check_sigs ? CheckSigs : NoCheckSigs, nullptr);

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status AddTextToStore(grpc::ServerContext*,
                        const nix::proto::AddTextToStoreRequest* request,
                        nix::proto::StorePath* response) override {
    return HandleExceptions(
        [&]() -> Status {
          PathSet references;
          for (const auto& ref : request->references()) {
            references.insert(ref);
          }
          auto path = store_->addTextToStore(request->name(),
                                             request->content(), references);
          response->set_path(path);
          return Status::OK;
        },
        __FUNCTION__);
  }

  Status BuildPaths(
      grpc::ServerContext*, const nix::proto::BuildPathsRequest* request,
      grpc::ServerWriter<nix::proto::BuildEvent>* writer) override {
    return HandleExceptions(
        [&]() -> Status {
          PathSet drvs;
          for (const auto& drv : request->drvs()) {
            drvs.insert(drv);
          }
          auto mode = BuildModeFrom(request->mode());

          if (!mode.has_value()) {
            return Status(grpc::StatusCode::INTERNAL, "Invalid build mode");
          }

          BuildLogStreambuf log_buffer(writer);
          std::ostream log_sink(&log_buffer);

          // TODO(grfn): If mode is repair and not trusted, we need to return an
          // error here (but we can't yet because we don't know anything about
          // trusted users)
          return nix::util::proto::AbslToGRPCStatus(
              store_->buildPaths(log_sink, drvs, mode.value()));
        },
        __FUNCTION__);
  }

  Status AddTempRoot(grpc::ServerContext*, const nix::proto::StorePath* request,
                     google::protobuf::Empty*) override {
    auto path = request->path();
    ASSERT_INPUT_STORE_PATH(path);

    return HandleExceptions(
        [&]() -> Status {
          store_->addTempRoot(path);
          return Status::OK;
        },
        __FUNCTION__);
  }

  Status AddIndirectRoot(grpc::ServerContext*,
                         const nix::proto::StorePath* request,
                         google::protobuf::Empty*) override {
    auto path = std::filesystem::canonical(request->path());
    ASSERT_INPUT_STORE_PATH(path);

    return HandleExceptions(
        [&]() -> Status {
          store_->addIndirectRoot(path);
          return Status::OK;
        },
        __FUNCTION__);
  }

  Status SyncWithGC(grpc::ServerContext*, const google::protobuf::Empty*,
                    google::protobuf::Empty*) override {
    return HandleExceptions(
        [&]() -> Status {
          store_->syncWithGC();
          return Status::OK;
        },
        __FUNCTION__);
  }

  Status FindRoots(grpc::ServerContext*, const google::protobuf::Empty*,
                   nix::proto::FindRootsResponse* response) override {
    return HandleExceptions(
        [&]() -> Status {
          auto roots = store_->findRoots(false);
          for (const auto& [target, links] : roots) {
            StorePaths link_paths;
            for (const auto& link : links) {
              link_paths.add_paths(link);
            }
            response->mutable_roots()->insert({target, link_paths});
          }

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status CollectGarbage(grpc::ServerContext*,
                        const proto::CollectGarbageRequest* request,
                        proto::CollectGarbageResponse* response) override {
    return HandleExceptions(
        [&]() -> Status {
          GCOptions options;
          auto action = GCActionFromProto(request->action());
          if (!action.has_value()) {
            return Status(grpc::StatusCode::INVALID_ARGUMENT,
                          "Invalid GC action");
          }

          options.action = action.value();
          for (const auto& path : request->paths_to_delete()) {
            options.pathsToDelete.insert(path);
          }
          options.ignoreLiveness = request->ignore_liveness();
          options.maxFreed = request->max_freed();

          if (options.ignoreLiveness) {
            return Status(grpc::StatusCode::INVALID_ARGUMENT,
                          "you are not allowed to ignore liveness");
          }

          GCResults results;
          store_->collectGarbage(options, results);

          for (const auto& path : results.paths) {
            response->add_deleted_paths(path);
          }
          response->set_bytes_freed(results.bytesFreed);

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QuerySubstitutablePathInfos(
      grpc::ServerContext*, const StorePaths* request,
      nix::proto::SubstitutablePathInfos* response) override {
    return HandleExceptions(
        [&]() -> Status {
          SubstitutablePathInfos infos;
          PathSet paths;
          for (const auto& path : request->paths()) {
            paths.insert(path);
          }
          store_->querySubstitutablePathInfos(paths, infos);
          for (const auto& [path, path_info] : infos) {
            auto proto_path_info = response->add_path_infos();
            proto_path_info->mutable_path()->set_path(path);
            proto_path_info->mutable_deriver()->set_path(path_info.deriver);
            for (const auto& ref : path_info.references) {
              proto_path_info->add_references(ref);
            }
            proto_path_info->set_download_size(path_info.downloadSize);
            proto_path_info->set_nar_size(path_info.narSize);
          }

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QueryValidDerivers(grpc::ServerContext* context,
                            const StorePath* request,
                            StorePaths* response) override {
    return HandleExceptions(
        [&]() -> Status {
          const auto& path = request->path();
          ASSERT_INPUT_STORE_PATH(path);

          PathSet paths = store_->queryValidDerivers(path);

          for (const auto& path : paths) {
            response->add_paths(path);
          }

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QueryDerivationOutputs(grpc::ServerContext* context,
                                const StorePath* request,
                                StorePaths* response) override {
    return HandleExceptions(
        [&]() -> Status {
          const auto& path = request->path();
          ASSERT_INPUT_STORE_PATH(path);

          PathSet paths = store_->queryDerivationOutputs(path);

          for (const auto& path : paths) {
            response->add_paths(path);
          }

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QueryAllValidPaths(grpc::ServerContext* context,
                            const google::protobuf::Empty* request,
                            StorePaths* response) override {
    return HandleExceptions(
        [&]() -> Status {
          const auto paths = store_->queryAllValidPaths();
          for (const auto& path : paths) {
            response->add_paths(path);
          }

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QueryPathInfo(grpc::ServerContext* context, const StorePath* request,
                       PathInfo* response) override {
    return HandleExceptions(
        [&]() -> Status {
          auto path = request->path();
          ASSERT_INPUT_STORE_PATH(path);

          response->mutable_path()->set_path(path);
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
          } catch (InvalidPath& e) {
            return Status(grpc::StatusCode::INVALID_ARGUMENT, e.msg());
          }
        },
        __FUNCTION__);
  }

  Status QueryDerivationOutputNames(
      grpc::ServerContext* context, const StorePath* request,
      nix::proto::DerivationOutputNames* response) override {
    return HandleExceptions(
        [&]() -> Status {
          auto path = request->path();
          ASSERT_INPUT_STORE_PATH(path);
          auto names = store_->queryDerivationOutputNames(path);
          for (const auto& name : names) {
            response->add_names(name);
          }

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QueryPathFromHashPart(grpc::ServerContext* context,
                               const nix::proto::HashPart* request,
                               StorePath* response) override {
    return HandleExceptions(
        [&]() -> Status {
          auto hash_part = request->hash_part();
          auto path = store_->queryPathFromHashPart(hash_part);
          ASSERT_INPUT_STORE_PATH(path);
          response->set_path(path);
          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QueryValidPaths(grpc::ServerContext* context,
                         const StorePaths* request,
                         StorePaths* response) override {
    return HandleExceptions(
        [&]() -> Status {
          std::set<Path> paths;
          for (const auto& path : request->paths()) {
            ASSERT_INPUT_STORE_PATH(path);
            paths.insert(path);
          }

          auto res = store_->queryValidPaths(paths);

          for (const auto& path : res) {
            response->add_paths(path);
          }

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QuerySubstitutablePaths(grpc::ServerContext* context,
                                 const StorePaths* request,
                                 StorePaths* response) override {
    return HandleExceptions(
        [&]() -> Status {
          std::set<Path> paths;
          for (const auto& path : request->paths()) {
            ASSERT_INPUT_STORE_PATH(path);
            paths.insert(path);
          }

          auto res = store_->querySubstitutablePaths(paths);

          for (const auto& path : res) {
            response->add_paths(path);
          }

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status OptimiseStore(grpc::ServerContext* context,
                       const google::protobuf::Empty* request,
                       google::protobuf::Empty* response) override {
    return HandleExceptions(
        [&]() -> Status {
          store_->optimiseStore();
          return Status::OK;
        },
        __FUNCTION__);
  }

  Status VerifyStore(grpc::ServerContext* context,
                     const nix::proto::VerifyStoreRequest* request,
                     nix::proto::VerifyStoreResponse* response) override {
    return HandleExceptions(
        [&]() -> Status {
          auto errors =
              store_->verifyStore(request->check_contents(),
                                  static_cast<RepairFlag>(request->repair()));

          response->set_errors(errors);

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status BuildDerivation(
      grpc::ServerContext* context,
      const nix::proto::BuildDerivationRequest* request,
      grpc::ServerWriter<nix::proto::BuildEvent>* writer) override {
    return HandleExceptions(
        [&]() -> Status {
          auto drv_path = request->drv_path().path();
          ASSERT_INPUT_STORE_PATH(drv_path);
          auto drv =
              BasicDerivation::from_proto(&request->derivation(), *store_);

          auto build_mode = nix::BuildModeFrom(request->build_mode());
          if (!build_mode) {
            return Status(grpc::StatusCode::INTERNAL, "Invalid build mode");
          }

          BuildResult res = store_->buildDerivation(drv_path, drv, *build_mode);

          proto::BuildResult proto_res{};
          proto_res.set_status(res.status_to_proto());

          if (!res.errorMsg.empty()) {
            proto_res.set_msg(res.errorMsg);
          }

          proto::BuildEvent event{};
          *event.mutable_result() = proto_res;

          writer->Write(event);
          return Status::OK;
        },
        __FUNCTION__);
  }

  Status AddSignatures(grpc::ServerContext* context,
                       const nix::proto::AddSignaturesRequest* request,
                       google::protobuf::Empty* response) override {
    return HandleExceptions(
        [&]() -> Status {
          auto path = request->path().path();
          ASSERT_INPUT_STORE_PATH(path);

          StringSet sigs;
          sigs.insert(request->sigs().sigs().begin(),
                      request->sigs().sigs().end());

          store_->addSignatures(path, sigs);

          return Status::OK;
        },
        __FUNCTION__);
  }

  Status QueryMissing(grpc::ServerContext* context, const StorePaths* request,
                      nix::proto::QueryMissingResponse* response) override {
    return HandleExceptions(
        [&]() -> Status {
          std::set<Path> targets;
          for (auto& path : request->paths()) {
            ASSERT_INPUT_STORE_PATH(path);
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
        },
        __FUNCTION__);
  };

  Status GetBuildLog(grpc::ServerContext* context, const StorePath* request,
                     proto::BuildLog* response) override {
    return HandleExceptions(
        [&]() -> Status {
          const auto log = store_->getBuildLog(request->path());
          if (log) {
            response->set_build_log(*log);
          }
          return Status::OK;
        },
        __FUNCTION__);
  }

 private:
  Status HandleExceptions(std::function<Status(void)> fn,
                          absl::string_view methodName) {
    try {
      return fn();
    } catch (Unsupported& e) {
      return Status(grpc::StatusCode::UNIMPLEMENTED,
                    absl::StrCat(methodName, " is not supported: ", e.what()));
    } catch (Error& e) {
      return Status(grpc::StatusCode::INTERNAL, e.what());
    }
    // add more specific Error-Status mappings above
  }

  ref<nix::Store> store_;
};

WorkerService::Service* NewWorkerService(nix::Store& store) {
  return new WorkerServiceImpl(store);
}

}  // namespace nix::daemon

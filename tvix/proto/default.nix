# Build protocol buffer definitions to ensure that protos are valid in
# CI. Note that the output of this build target is not actually used
# anywhere, it just functions as a CI check for now.
{ pkgs, depot, ... }:

let
  # Only select the protos from $out/tvix/proto, so the import paths relative
  # to the root work without importing all of depot into the store on every
  # build.
  onlyProtos = depot.nix.sparseTree depot.path.origSrc [
    ./castore.proto
    ./evaluator.proto
    ./pathinfo.proto
    ./rpc_blobstore.proto
  ];
in

pkgs.runCommand "tvix-cc-proto" { } ''
  mkdir -p $out
  ${pkgs.protobuf}/bin/protoc -I ${onlyProtos}/ ${onlyProtos}/tvix/proto/*.proto --cpp_out=$out
''

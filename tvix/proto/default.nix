# Build protocol buffer definitions to ensure that protos are valid in
# CI. Note that the output of this build target is not actually used
# anywhere, it just functions as a CI check for now.
{ pkgs, ... }:

# Copy the protos into $out/tvix/proto, so the import paths relative to the
# root work without importing all of depot into the store on every build.
pkgs.runCommand "tvix-cc-proto" { } ''
  mkdir -p $out/tvix/proto
  cp -R ${./.}/*.proto $out/tvix/proto
  ${pkgs.protobuf}/bin/protoc -I$out $out/tvix/proto/*.proto --cpp_out=$out
''

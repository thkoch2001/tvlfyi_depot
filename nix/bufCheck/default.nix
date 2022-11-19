# Check protobuf syntax and breaking.
#
{ depot, pkgs, ... }:

pkgs.writeShellScriptBin "ci-buf-check" ''
  export PATH="$PATH:${pkgs.lib.makeBinPath [ pkgs.buf pkgs.protoc-gen-go pkgs.protoc-gen-go-grpc ]}"
  buf lint .

  # Run buf generate, and bail out if generated files are changed.
  buf generate --path tvix/store/protos
  # Check if any files have changed
  if [[ -n "$(git status --porcelain -unormal)" ]]; then
      echo "-----------------------------"
      echo ".pb.go files need to be updated"
      echo "-----------------------------"
      git status -unormal
      exit 1
  fi

  # Report-only
  buf breaking . --against "./.git#ref=HEAD~1" || true
''

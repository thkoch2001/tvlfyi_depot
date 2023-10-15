{ pkgs, depot, ... }:

let
  npmBase = {
    src = pkgs.emptyDirectory;
    postPatch = ''
      cp ${./package.json} ./package.json
      cp ${./package-lock.json} ./package-lock.json
    '';
    npmDepsHash = "sha256:0b2blmfs664ig7nqb78glbkvf8zlqh6kn407fxwvbziqmd375xvc";
  };
in rec {
  protodir = pkgs.runCommand "tvix-protos" {
    protodir = pkgs.lib.sources.sourceFilesBySuffices ../. [".proto"];
  } ''
    cd $protodir
    find . -type f | while read f; do
      mkdir -p "$out/tvix/$(dirname $f)"
      cp "$f" "$out/tvix/$f"
    done
  '';
  protodir-js = pkgs.buildNpmPackage (npmBase // {
    name = "tvix-protodir-js";

    nativeBuildInputs = with pkgs; [ protobuf protoc-gen-grpc-web depot.third_party.protoc-gen-js ];
    inherit protodir;

    dontNpmBuild = true;
    postBuild = ''
      pushd $protodir
      mkdir $out
      find . -type f | while read f; do
        protoc -I . "$f" --js_out=import_style=commonjs:$out --grpc-web_out=import_style=commonjs,mode=grpcwebtext:$out
      done
      popd
    '';

    dontNpmInstall = true;
  });
  protoinclude-js = pkgs.runCommand "tvix-protoinclude-js" {
    head = ./head.js;
    inherit protodir;
    protodirjs = protodir-js;
  } ''
    mkdir $out
    cat $head > $out/protoinclude.js

    pushd $protodirjs
    find . -type f | while read f; do
      f="$(echo $f | sed -e 's,^[.]/,,')"
      grpc_web_name="$(echo $f | sed -e 's,_pb.js$,_grpc_web_pb.js,')"
      # If we have a corresponding gRPC-Web version, skip this
      test -f $grpc_web_name && continue

      proto_name="$(echo $f | sed -e 's,_grpc_web_pb.js$,.proto,' -e 's,_pb.js$,.proto,')"
      package="$(grep -Po '(?<=^package )[^;]+' "$protodir/$proto_name")"
      echo "loadProto(proto, "'"'"''${package}"'"'", require('./protos/''${f}'));" >> $out/protoinclude.js
    done
    popd
  '';
  protobundle-js = pkgs.buildNpmPackage (npmBase // {
    name = "tvix-protobundle-js";

    dontNpmBuild = true;
    postBuild = ''
      cp ${protoinclude-js}/protoinclude.js protoinclude.js
      cp -R ${protodir-js} protos

      ls ./protos/tvix/proto/evaluator_grpc_web_pb.js
      ls -la ./protos/tvix/proto/evaluator_grpc_web_pb.js

      mkdir $out
      node_modules/.bin/browserify -o $out/protobundle.js protoinclude.js
    '';

    dontNpmInstall = true;
  });
}

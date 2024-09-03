{ depot, pkgs, ... }:

let
  em = depot.tools.eaglemode;
  emSrc = pkgs.srcOnly pkgs.em;
in
(em.buildPlugin {
  name = "avif";
  version = "canon";
  src = ./.;
  target = "PlAvif";
}).overrideAttrs
  ({ buildInputs ? [ ], nativeBuildInputs ? [ ], ... }: {
    buildInputs = buildInputs ++ [ pkgs.libavif ];
    nativeBuildInputs = nativeBuildInputs ++ [ pkgs.pkg-config ];
  })

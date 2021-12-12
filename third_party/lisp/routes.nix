{ depot, pkgs, ... }:

let

  src = pkgs.applyPatches {
    name = "routes-source";
    src = pkgs.fetchFromGitHub {
      owner = "archimag";
      repo = "cl-routes";
      rev = "1b79e85aa653e1ec87e21ca745abe51547866fa9";
      sha256 = "1zpk3cp2v8hm50ppjl10yxr437vv4552r8hylvizglzrq2ibsbr1";
    };

    patches = [
      (pkgs.fetchpatch {
        name = "fix-build-with-ccl.patch";
        url =
          "https://github.com/archimag/cl-routes/commit/2296cdc316ef8e34310f2718b5d35a30040deee0.patch";
        sha256 = "007c19kmymalam3v6l6y2qzch8xs3xnphrcclk1jrpggvigcmhax";
      })
    ];
  };

in depot.nix.buildLisp.library {
  name = "routes";

  deps = with depot.third_party.lisp; [ puri iterate split-sequence ];

  srcs = map (f: src + ("/src/" + f)) [
    "package.lisp"
    "uri-template.lisp"
    "route.lisp"
    "mapper.lisp"
  ];
}

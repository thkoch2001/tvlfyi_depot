{ pkgs, lib, ... }:

let inherit (pkgs) cmake llvmPackages;
in llvmPackages.libcxxStdenv.mkDerivation rec {
  pname = "abseil-cpp";
  version = "20200519-768eb2ca";
  src = ./.;
  nativeBuildInputs = [ cmake ];

  meta = with lib; {
    description = "An open-source collection of C++ code designed to augment the C++ standard library";
    homepage = https://abseil.io/;
    license = licenses.asl20;
    maintainers = [ maintainers.andersk ];
  };
}

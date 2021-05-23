{ pkgs, lib, ... }:

let inherit (pkgs) cmake llvmPackages_11;
in llvmPackages_11.libcxxStdenv.mkDerivation rec {
  pname = "abseil-cpp";
  version = "20200519-768eb2ca+tvl-1";
  src = ./.;
  nativeBuildInputs = [ cmake ];
  # TODO: run tests
  # doCheck = true;

  cmakeFlags = [
    "-DCMAKE_CXX_STANDARD=17"
    #"-DABSL_RUN_TESTS=1"
  ];

  meta = with lib; {
    description = "An open-source collection of C++ code designed to augment the C++ standard library";
    homepage = https://abseil.io/;
    license = licenses.asl20;
    maintainers = [ maintainers.andersk ];
  };
}

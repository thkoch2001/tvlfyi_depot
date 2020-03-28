{ perl, stdenv, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "alsi";
  pname = "alsi";
  version = "0.4.8";

  src = fetchFromGitHub {
    owner = "trizen";
    repo = "alsi";
    rev = "fe2a925caad38d4cc7afe10d74ba60c5db09ee66";
    sha256 = "060xlalfclrda5f1h3svj4v2gr19mdrsc62vrg7hgii0f3lib7j5";
  };

  buildInputs = [
    (perl.withPackages (ps: with ps; [ DataDump ]))
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp alsi $out/bin/alsi
  '';
}

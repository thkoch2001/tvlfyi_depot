{ pkgs
, lib
, ...
}:

let
  inherit (pkgs)
    alsa-utils
    fetchFromGitHub
    stdenv
    ;
in

stdenv.mkDerivation (finalAttrs: {
  pname = "bruh";
  version = "2.1";

  src =
    with finalAttrs;
    fetchFromGitHub {
      owner = "kejpies";
      repo = pname;
      rev = version;
      hash = "sha256-Uw6Qes0IZkkfBchFnvnX9l1ZG5T5pyExmV7yUJLPOJ0=";
    };

  postPatch = ''
    substituteInPlace bruh.c \
      --replace-fail "aplay" "${alsa-utils}/bin/aplay"
  '';

  makeFlags = [ "PREFIX=$(out)" ];

  meta = with lib; {
    description = "Bruh sound, but as a program";
    inherit (finalAttrs.src.meta) homepage;
    license = licenses.gpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ azahi ];
    mainProgram = "bruh";
  };
})

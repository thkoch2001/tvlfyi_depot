{ depot, pkgs, ... }:

let
  inherit (pkgs) fontconfig texlive stdenv imagemagick;

  tex = texlive.combine {
    inherit (texlive)
      babel
      babel-russian
      beamer
      beamertheme-metropolis
      etoolbox
      euenc
      extsizes
      fontspec
      listings
      xetex
      minted
      ms
      pgfopts
      scheme-basic
      translator;
  };

in
stdenv.mkDerivation {
  name = "progmsk-tvix-eval";
  src = ./.;

  nativeBuildInputs = [ tex imagemagick fontconfig ];

  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = with pkgs; [ jetbrains-mono fira fira-code fira-mono ];
  };

  buildPhase = ''
    # LaTeX needs a cache folder in /home/ ...
    mkdir home
    export HOME=$PWD/home

    # webp images can't be included directly, need to convert to PNG
    convert ${depot.tvix.website}/tvix-logo.webp tvix-logo.png

    # As usual, TeX needs to be run twice ...
    ${tex}/bin/xelatex presentation.tex
    ${tex}/bin/xelatex presentation.tex
  '';

  installPhase = ''
    mkdir -p $out
    cp presentation.pdf $out/
    cp $src/presentation.pdfpc $out/
  '';
}

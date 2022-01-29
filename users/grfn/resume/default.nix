{ pkgs, ... }:

with pkgs.lib;

pkgs.runCommandNoCC "resume.pdf"
{
  buildInputs = [
    (pkgs.texlive.combine {
      inherit (pkgs.texlive)
        capt-of
        collection-fontsrecommended
        enumitem
        etoolbox
        fancyvrb
        float
        fncychap
        framed
        l3packages
        microtype
        needspace
        parskip
        scheme-basic
        tabulary
        titlesec
        ulem
        upquote
        varwidth
        wrapfig
        xcolor
        ;
    })
  ];
} ''
  cp ${builtins.filterSource (path: type:
    type == "regular" &&
    any (ext: hasSuffix ext path) [".sty" ".cls" ".tex" ".png"]
  ) ./.}/* .
  pdflatex ./resume.tex
  cp resume.pdf $out
''

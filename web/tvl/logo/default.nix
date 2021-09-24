# Creates an output containing the logo in SVG format (animated and
# static, one for each background colour) and without animations in
# PNG.
{ depot, lib, pkgs, ... }:

let
  palette = {
    purple = "#CC99C9";
    blue = "#9EC1CF";
    green = "#9EE09E";
    yellow = "#FDFD97";
    orange = "#FEB144";
    red = "#FF6663";
  };

  logoShapes = builtins.readFile ./logo-shapes.svg;
  logoSvg = style: ''
    <svg xmlns="http://www.w3.org/2000/svg" xml:space="preserve" viewBox="420 860 1640 1500">
      <style>${style}</style>
      ${logoShapes}
    </svg>
  '';

  staticCss = colour: ''
    #armchair-background {
      fill: ${colour};
    }
  '';

  # Create an animated CSS that equally spreads out the colours over
  # the animation duration (1min).
  animatedCss = colours: let
    # Calculate at which percentage offset each colour should appear.
    stepSize = 100 / ((builtins.length colours) - 1);
    frames = lib.imap0 (idx: colour: { inherit colour; at = idx * stepSize; }) colours;
    frameCss = frame: "${toString frame.at}% { fill: ${frame.colour}; }";
  in ''
    #armchair-background {
      animation: 60s infinite alternate armchairPalette;
    }

    @keyframes armchairPalette {
    ${lib.concatStringsSep "\n" (map frameCss frames)}
    }
  '';

in depot.nix.utils.drvTargets(lib.fix (self: {
  inherit palette;

  # Create a TVL logo SVG with the specified static armchair colour.
  staticLogoSvg = colour: pkgs.writeText "logo.svg" (logoSvg (staticCss colour));

  # Create a TVL logo SVG with the specified animated armchair colour set.
  animatedLogoSvg = colours: pkgs.writeText "logo.svg" (logoSvg (animatedCss colours));

  # Create a PNG of the TVL logo with the specified static armchair colour and DPI.
  logoPng = colour: dpi: pkgs.runCommandNoCC "logo.png" {} ''
    ${pkgs.inkscape}/bin/inkscape \
      --export-area-drawing \
      --export-background-opacity 0 \
      --export-dpi ${toString dpi} \
      ${self.staticLogoSvg colour} -o $out
  '';

  # Animated SVG logo with all colours.
  pastelRainbow = self.animatedLogoSvg (lib.attrValues palette);
}

# Add individual outputs for static logos of each colour.
// (lib.mapAttrs' (k: v: lib.nameValuePair "${k}Png" (self.logoPng v 96)) palette)))

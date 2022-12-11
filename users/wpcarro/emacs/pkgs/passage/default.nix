{ pkgs, depot, ... }:

 pkgs.callPackage
   ({ emacsPackages }:
     emacsPackages.trivialBuild {
       pname = "passage";
       version = "1.0.0";
       src = ./passage.el;
       packageRequires = (with emacsPackages; [ dash f s ]);
     }
   ) { }

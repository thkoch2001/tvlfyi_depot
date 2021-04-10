# Pin a working combination of telega.el & tdlib, as these are totally
# out-of-sync in nixpkgs.
{ pkgs, ... }:

let
  tdlib = pkgs.tdlib.overrideAttrs(old: {
    version = "1.6.6";
    src = pkgs.fetchFromGitHub {
      owner = "tdlib";
      repo = "td";
      rev = "c78fbe4bc5e31395e08f916816704e8051f27296"; # 1.6.6
      sha256 = "18ny990cvnwj5sd5jp49n0jn8b8fa8iszw4vxvsqdnw00srw0ggd";
    };
  });
# Emacs packages use some sort of fixed point override scheme and
# don't expose individual overrides in the override-functor, hence the
# mess below.
in pkgs.emacsPackages.telega.overrideAttrs(old: {
  buildInputs = [ tdlib ] ++
    (builtins.filter (p: !(p ? pname) || p.pname != "tdlib") old.buildInputs);
})

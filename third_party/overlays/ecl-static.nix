{ ... }:

self: super:

{
  # Statically linked ECL with statically linked dependencies.
  # Works quite well, but solving this properly in a nixpkgs
  # context will require figuring out cross compilation (for
  # pkgsStatic), so we're gonna use this override for now.
  #
  # Note that ecl-static does mean that we have things
  # statically linked against GMP and ECL which are LGPL.
  # I believe this should be alright: The way ppl are gonna
  # interact with the distributed binaries (i. e. the binary
  # cache) is Nix in the depot monorepo, so the separability
  # requirement should be satisfied: Source code or overriding
  # would be available as ways to swap out the used GMP in the
  # program.
  # See https://www.gnu.org/licenses/gpl-faq.en.html#LGPLStaticVsDynamic
  ecl-static = (super.pkgsMusl.ecl.override {
    inherit (self.pkgsStatic) gmp libffi boehmgc;
  }).overrideAttrs (drv: {
    configureFlags = drv.configureFlags ++ [
      "--disable-shared"
    ];
  });
}

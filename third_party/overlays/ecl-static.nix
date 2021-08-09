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
    # Patches that make .fasc files concatenable again
    patches = drv.patches ++ [
      (self.fetchpatch {
        name = "make-bytecode-fasl-concatenatable-1.patch";
        url = "https://gitlab.com/embeddable-common-lisp/ecl/-/commit/fbb75a0fc524e3280d89d8abf3be2ee9924955c8.patch";
        sha256 = "0k6cx1bh835rl0j0wbbi5nj0aa2rwbyfyz5q2jw643iqc62l16kv";
      })
      (self.fetchpatch {
        name = "make-bytecode-fasl-concatenatable-2.patch";
        url = "https://gitlab.com/embeddable-common-lisp/ecl/-/commit/a8b1c0da43f89800d09c23a27832d0b4c9dcc1e8.patch";
        sha256 = "18hl79lss0dxglpa34hszqb6ajvs8rs4b4g1qlrqrvgh1gs667n0";
      })
    ];
    configureFlags = drv.configureFlags ++ [
      "--disable-shared"
      "--with-dffi=no" # will fail at runtime anyways if statically linked
    ];
  });
}

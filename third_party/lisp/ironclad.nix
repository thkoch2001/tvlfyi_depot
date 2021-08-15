{ depot, pkgs, ...}:

let
  inherit (pkgs) runCommand;
  inherit (depot.nix.buildLisp) bundled;
  src = pkgs.fetchFromGitHub {
    owner = "sharplispers";
    repo = "ironclad";
    rev = "c3aa33080621abc10fdb0f34acc4655cc4e982a6";
    sha256 = "0k4bib9mbrzalbl9ivkw4a7g4c7bbad1l5jw4pzkifqszy2swkr5";
  };

in depot.nix.buildLisp.library {
  name = "ironclad";

  deps = with depot.third_party.lisp; [
    (bundled "asdf")
    (bundled "sb-rotate-byte")
    (bundled "sb-posix")
    alexandria
    bordeaux-threads
    nibbles
  ];

  srcs = [
    "${src}/ironclad.asd"
    # TODO(grfn): Figure out how to get this compiling with the assembly
    # optimization eventually - see https://cl.tvl.fyi/c/depot/+/1333
    (runCommand "package.lisp" {} ''
      substitute ${src}/src/package.lisp $out \
        --replace \#-ecl-bytecmp "" \
        --replace '(pushnew :ironclad-assembly *features*)' ""
    '')
  ] ++ (map (f: src + ("/src/" + f)) [
    "macro-utils.lisp"

    "opt/sbcl/fndb.lisp"
    "opt/sbcl/cpu-features.lisp"
    "opt/sbcl/x86oid-vm.lisp"

    "common.lisp"
    "conditions.lisp"
    "generic.lisp"
    "util.lisp"

    "ciphers/padding.lisp"
    "ciphers/cipher.lisp"
    "ciphers/chacha.lisp"
    "ciphers/modes.lisp"
    "ciphers/salsa20.lisp"
    "ciphers/xchacha.lisp"
    "ciphers/xsalsa20.lisp"
    "ciphers/aes.lisp"
    "ciphers/arcfour.lisp"
    "ciphers/arcfour.lisp"
    "ciphers/aria.lisp"
    "ciphers/blowfish.lisp"
    "ciphers/camellia.lisp"
    "ciphers/cast5.lisp"
    "ciphers/des.lisp"
    "ciphers/idea.lisp"
    "ciphers/keystream.lisp"
    "ciphers/kalyna.lisp"
    "ciphers/kuznyechik.lisp"
    "ciphers/make-cipher.lisp"
    "ciphers/misty1.lisp"
    "ciphers/rc2.lisp"
    "ciphers/rc5.lisp"
    "ciphers/rc6.lisp"
    "ciphers/seed.lisp"
    "ciphers/serpent.lisp"
    "ciphers/sm4.lisp"
    "ciphers/sosemanuk.lisp"
    "ciphers/square.lisp"
    "ciphers/tea.lisp"
    "ciphers/threefish.lisp"
    "ciphers/twofish.lisp"
    "ciphers/xor.lisp"
    "ciphers/xtea.lisp"

    "digests/digest.lisp"
    "digests/adler32.lisp"
    "digests/blake2.lisp"
    "digests/blake2s.lisp"
    "digests/crc24.lisp"
    "digests/crc32.lisp"
    "digests/groestl.lisp"
    "digests/jh.lisp"
    "digests/kupyna.lisp"
    "digests/md2.lisp"
    "digests/md4.lisp"
    "digests/md5.lisp"
    "digests/md5-lispworks-int32.lisp"
    "digests/ripemd-128.lisp"
    "digests/ripemd-160.lisp"
    "digests/sha1.lisp"
    "digests/sha256.lisp"
    "digests/sha3.lisp"
    "digests/sha512.lisp"
    "digests/skein.lisp"
    "digests/sm3.lisp"
    "digests/streebog.lisp"
    "digests/tiger.lisp"
    "digests/tree-hash.lisp"
    "digests/whirlpool.lisp"

    "prng/prng.lisp"
    "prng/generator.lisp"
    "prng/fortuna.lisp"
    "prng/os-prng.lisp"

    "math.lisp"

    "macs/mac.lisp"
    "macs/blake2-mac.lisp"
    "macs/blake2s-mac.lisp"
    "macs/cmac.lisp"
    "macs/hmac.lisp"
    "macs/gmac.lisp"
    "macs/poly1305.lisp"
    "macs/siphash.lisp"
    "macs/skein-mac.lisp"

    "kdf/kdf-common.lisp"
    "kdf/argon2.lisp"
    "kdf/password-hash.lisp"
    "kdf/pkcs5.lisp"
    "kdf/scrypt.lisp"
    "kdf/hmac.lisp"

    "aead/aead.lisp"
    "aead/eax.lisp"
    "aead/etm.lisp"
    "aead/gcm.lisp"

    "public-key/public-key.lisp"
    "public-key/curve25519.lisp"
    "public-key/curve448.lisp"
    "public-key/dsa.lisp"
    "public-key/ed25519.lisp"
    "public-key/ed448.lisp"
    "public-key/elgamal.lisp"
    "public-key/pkcs1.lisp"
    "public-key/rsa.lisp"
  ]);
}

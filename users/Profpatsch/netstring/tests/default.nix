{ depot, lib, pkgs, ... }:

let

  python-netstring-test = depot.users.Profpatsch.writers.python3
    {
      name = "python-netstring-test";
      libraries = p: [
        depot.users.Profpatsch.netstring.python-netstring
      ];
    } ''
    import netstring

    def assEq(left, right):
      assert left == right, "{} /= {}".format(str(left), str(right))

    assEq(
      netstring.read_netstring(b"""${depot.nix.netstring.fromString "hi!"}"""),
      (b"hi!", b"")
    )

    assEq(
      netstring.read_netstring_key_val(
        b"""${depot.nix.netstring.attrsToKeyValList { foo = "42"; }}"""
      ),
      (b'foo', b'42', b"")
    )

    assEq(
      netstring.read_netstring_key_val_list(
        b"""${depot.nix.netstring.attrsToKeyValList { foo = "42"; bar = "hi"; }}"""
      ),
      { b'foo': b'42', b'bar': b'hi' }
    )
  '';

  rust-netstring-test = depot.nix.writers.rustSimple
    {
      name = "rust-netstring-test";
      dependencies = [
        depot.users.Profpatsch.netstring.rust-netstring
      ];
    } ''
    extern crate netstring;

    fn main() {
      assert_eq!(
        std::str::from_utf8(&netstring::to_netstring(b"hello")).unwrap(),
        r##"${depot.nix.netstring.fromString "hello"}"##
      );
      assert_eq!(
        std::str::from_utf8(&netstring::to_netstring("こんにちは".as_bytes())).unwrap(),
        r##"${depot.nix.netstring.fromString "こんにちは"}"##
      );
    }
  '';

in
depot.nix.readTree.drvTargets {
  inherit
    python-netstring-test
    rust-netstring-test
    ;
}

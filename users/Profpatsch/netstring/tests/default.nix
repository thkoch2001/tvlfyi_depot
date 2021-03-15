{ depot, lib, pkgs, ... }:

let

  inherit (depot.users.Profpatsch.netstring)
    toNetstring
    toNetstringKeyVal
    ;

  python-netstring-test = depot.users.Profpatsch.writers.python3 {
    name = "python-netstring-test";
    libraries = p: [
      python-netstring
    ];
  } ''
    import netstring

    def assEq(left, right):
      assert left == right, "{} /= {}".format(str(left), str(right))

    assEq(
      netstring.read_netstring(b"""${toNetstring "hi!"}"""),
      (b"hi!", b"")
    )

    assEq(
      netstring.read_netstring_key_val(
        b"""${toNetstringKeyVal { foo = "42"; }}"""
      ),
      (b'foo', b'42', b"")
    )

    assEq(
      netstring.read_netstring_key_val_list(
        b"""${toNetstringKeyVal { foo = "42"; bar = "hi"; }}"""
      ),
      { b'foo': b'42', b'bar': b'hi' }
    )
  '';

  rust-netstring-test = depot.users.Profpatsch.writers.rustSimple {
    name = "rust-netstring-test";
    dependencies = [
      rust-netstring
    ];
  } ''
    extern crate netstring;

    fn main() {
      assert_eq!(
        std::str::from_utf8(&netstring::to_netstring(b"hello")).unwrap(),
        r##"${toNetstring "hello"}"##
      );
      assert_eq!(
        std::str::from_utf8(&netstring::to_netstring("こんにちは".as_bytes())).unwrap(),
        r##"${toNetstring "こんにちは"}"##
      );
    }
  '';

in depot.nix.utils.drvTargets {
  inherit
    python-netstring-test
    rust-netstring-test
    ;
}

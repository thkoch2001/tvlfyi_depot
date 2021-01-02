{ depot, lib, pkgs, python-netstring, toNetstring, toNetstringKeyVal }:

let
  imports = {
    inherit (depot.users.Profpatsch)
      writers
      ;
  };

  python-netstring-test = imports.writers.python3 {
    name = "python-netstring";
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

in {
  inherit
    python-netstring-test
    ;
}

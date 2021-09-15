{ depot, ... }:

let

  inherit (depot.nix.runTestsuite)
    it
    assertEq
    runNintTestsuite
    ;

  inherit (depot.users.sterni.nix)
    url
    ;

  checkEncoding = args: { left, right }:
    assertEq "encode ${builtins.toJSON left} == ${builtins.toJSON right}"
      (url.encode args left) right;

  checkDecoding = { left, right }:
  assertEq "${builtins.toJSON left} == decode ${builtins.toJSON right}"
    (url.decode left) right;

  unreserved = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_~";

  encodeExpected = [
    { left = "Laguna Beach"; right = "Laguna%20Beach"; }
    { left = "👾 Exterminate!"; right = "%F0%9F%91%BE%20Exterminate%21"; }
    { left = unreserved; right = unreserved; }
    {
      left = "`!@#$%^&*()+={}[]:;'\\|<>,?/ \"";
      right = "%60%21%40%23%24%25%5E%26%2A%28%29%2B%3D%7B%7D%5B%5D%3A%3B%27%5C%7C%3C%3E%2C%3F%2F%20%22";
    }
  ];

  testEncode = it "checks url.encode"
    (builtins.map (checkEncoding {}) encodeExpected);

  testDecode = it "checks url.decode"
    (builtins.map checkDecoding encodeExpected);

  testLeaveReserved = it "checks that leaveReserved is like id for valid URLs"
    (builtins.map (x: checkEncoding { leaveReserved = true; } { left = x; right = x; }) [
      "ftp://ftp.is.co.za/rfc/rfc1808.txt"
      "http://www.ietf.org/rfc/rfc2396.txt"
      "ldap://[2001:db8::7]/c=GB?objectClass?one"
      "mailto:John.Doe@example.com"
      "news:comp.infosystems.www.servers.unix"
      "tel:+1-816-555-1212"
      "telnet://192.0.2.16:80/"
      "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
    ]);
in
  runNintTestsuite "nix.url" [
    testEncode
    testLeaveReserved
  ]

{ depot, ... }:

let

  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    ;

  inherit (depot.users.sterni.nix)
    int
    string
    ;

  testBounds = it "checks minBound and maxBound" [
    # this is gonna blow up in my face because
    # integer overflow is undefined behavior in
    # C++, so most likely anything could happen?
    (assertEq "maxBound is the maxBound" true
      (int.maxBound + 1 < int.maxBound))
    (assertEq "minBound is the minBound" true
      (int.minBound - 1 > int.minBound))
    (assertEq "maxBound overflows to minBound"
      (int.maxBound + 1)
      int.minBound)
    (assertEq "minBound overflows to maxBound"
      (int.minBound - 1)
      int.maxBound)
  ];

  expectedBytes = [
    "00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0A" "0B" "0C" "0D" "0E" "0F"
    "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "1A" "1B" "1C" "1D" "1E" "1F"
    "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "2A" "2B" "2C" "2D" "2E" "2F"
    "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "3A" "3B" "3C" "3D" "3E" "3F"
    "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" "4A" "4B" "4C" "4D" "4E" "4F"
    "50" "51" "52" "53" "54" "55" "56" "57" "58" "59" "5A" "5B" "5C" "5D" "5E" "5F"
    "60" "61" "62" "63" "64" "65" "66" "67" "68" "69" "6A" "6B" "6C" "6D" "6E" "6F"
    "70" "71" "72" "73" "74" "75" "76" "77" "78" "79" "7A" "7B" "7C" "7D" "7E" "7F"
    "80" "81" "82" "83" "84" "85" "86" "87" "88" "89" "8A" "8B" "8C" "8D" "8E" "8F"
    "90" "91" "92" "93" "94" "95" "96" "97" "98" "99" "9A" "9B" "9C" "9D" "9E" "9F"
    "A0" "A1" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9" "AA" "AB" "AC" "AD" "AE" "AF"
    "B0" "B1" "B2" "B3" "B4" "B5" "B6" "B7" "B8" "B9" "BA" "BB" "BC" "BD" "BE" "BF"
    "C0" "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9" "CA" "CB" "CC" "CD" "CE" "CF"
    "D0" "D1" "D2" "D3" "D4" "D5" "D6" "D7" "D8" "D9" "DA" "DB" "DC" "DD" "DE" "DF"
    "E0" "E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8" "E9" "EA" "EB" "EC" "ED" "EE" "EF"
    "F0" "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "FA" "FB" "FC" "FD" "FE" "FF"
  ];

  hexByte = i: string.fit { width = 2; char = "0"; } (int.toHex i);

  hexInts = [
    { left = 0; right = "0"; }
    { left = 1; right = "1"; }
    { left = 11; right = "B"; }
    { left = 123; right = "7B"; }
    { left = 9000; right = "2328"; }
    { left = 2323; right = "913"; }
    { left = 4096; right = "1000"; }
    { left = int.maxBound; right = "7FFFFFFFFFFFFFFF"; }
    { left = int.minBound; right = "-8000000000000000"; }
  ];

  testHex = it "checks conversion to hex" [
    (assertEq "all bytes converted to hex" expectedBytes
      (builtins.map hexByte (builtins.genList (i: i) 256)))
    (assertEq "all bytes converted from hex" expectedBytes
      (builtins.map hexByte (builtins.genList (i: i) 256)))
    (assertEq "non fixed width integers converted to hex"
      (builtins.map (x: int.toHex x.left) hexInts)
      (builtins.map (x: x.right) hexInts))
    (assertEq "non fixed width integers converted from hex"
      (builtins.map (x: int.fromHex x.right) hexInts)
      (builtins.map (x: x.left) hexInts))
  ];

  testBasic = it "checks basic int operations" [
    (assertEq "122 is even" (int.even 122 && !(int.odd 122)) true)
    (assertEq "123 is odd" (int.odd 123 && !(int.even 123)) true)
    (assertEq "abs -4959" (int.abs (-4959)) 4959)
  ];

  expNumbers = [
    { left = -3; right = 0.125; }
    { left = -2; right = 0.25; }
    { left = -1; right = 0.5; }
    { left = 0; right = 1; }
    { left = 1; right = 2; }
    { left = 2; right = 4; }
    { left = 3; right = 8; }
    { left = 4; right = 16; }
    { left = 5; right = 32; }
    { left = 16; right = 65536; }
  ];

  testExp = it "checks exponentiation" [
    (assertEq "2 ^ n"
      (builtins.map (x: int.exp 2 x.left) expNumbers)
      (builtins.map (x: x.right) expNumbers))
  ];

  testBit = it "checks bitwise operations" [
    # TODO(sterni)
  ];

  testDivMod = it "checks integer division and modulo" [
    # TODO(sterni)
  ];

in
  runTestsuite "nix.int" [
    testBounds
    testHex
    testBasic
    testExp
    testBit
    testDivMod
  ]

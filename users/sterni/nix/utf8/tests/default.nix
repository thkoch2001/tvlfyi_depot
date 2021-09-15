{ depot, pkgs, lib, ... }:

let

  inherit (pkgs)
    runCommandLocal
    ;

  inherit (depot.nix.runTestsuite)
    runNintTestsuite
    it
    assertEq
    assertThrows
    assertDoesNotThrow
    ;

  inherit (depot.nix.writers)
    rustSimple
    ;

  inherit (depot.users.sterni.nix)
    int
    utf8
    string
    char
    ;

  rustDecoder = rustSimple {
    name = "utf8-decode";
  } ''
    use std::io::{self, Read};
    fn main() -> std::io::Result<()> {
      let mut buffer = String::new();
      io::stdin().read_to_string(&mut buffer)?;

      print!("[ ");

      for c in buffer.chars() {
        print!("{} ", u32::from(c));
      }

      print!("]");

      Ok(())
    }
  '';

  rustDecode = s:
    let
      expr = runCommandLocal "${s}-decoded" {} ''
        printf '%s' ${lib.escapeShellArg s} | ${rustDecoder} > $out
      '';
    in import expr;

  hexDecode = l:
    utf8.decode (string.fromBytes (builtins.map int.fromHex l));

  testFailures = it "checks UTF-8 decoding failures" [
    (assertThrows "emtpy bytestring throws" (utf8.decode ""))
    (assertThrows "truncated UTF-8 string throws" (hexDecode [ "F0" "9F" ]))
    # examples from The Unicode Standard
    (assertThrows "ill-formed: C0 AF" (hexDecode [ "C0" "AF" ]))
    (assertThrows "ill-formed: E0 9F 80" (hexDecode [ "E0" "9F" "80" ]))
    (assertEq "well-formed: F4 80 83 92" (hexDecode [ "F4" "80" "83" "92" ]) [ 1048786 ])
  ];

  testAscii = it "checks decoding of ascii strings"
    (builtins.map (s: assertEq "ASCII decoding is equal to UTF-8 decoding for \"${s}\""
      (string.toBytes s) (utf8.decode s)) [
        "foo bar"
        "hello\nworld"
        "carriage\r\nreturn"
        "1238398494829304 []<><>({})[]!!)"
        (string.take 127 char.allChars)
      ]);

  randomUnicode = [
    "🥰👨‍👨‍👧‍👦🐈‍⬛👩🏽‍🦰"
    # https://kermitproject.org/utf8.html
    "ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ"
    "An preost wes on leoden, Laȝamon was ihoten"
    "Sîne klâwen durh die wolken sint geslagen,"
    "Τὴ γλῶσσα μοῦ ἔδωσαν ἑλληνικὴ"
    "На берегу пустынных волн"
    "ვეპხის ტყაოსანი შოთა რუსთაველი"
    "யாமறிந்த மொழிகளிலே தமிழ்மொழி போல் இனிதாவது எங்கும் காணோம், "
    "ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸು "
  ];

  # https://kermitproject.org/utf8.html
  glassSentences = [
    "Euro Symbol: €."
    "Greek: Μπορώ να φάω σπασμένα γυαλιά χωρίς να πάθω τίποτα."
    "Íslenska / Icelandic: Ég get etið gler án þess að meiða mig."
    "Polish: Mogę jeść szkło, i mi nie szkodzi."
    "Romanian: Pot să mănânc sticlă și ea nu mă rănește."
    "Ukrainian: Я можу їсти шкло, й воно мені не пошкодить."
    "Armenian: Կրնամ ապակի ուտել և ինծի անհանգիստ չըներ։"
    "Georgian: მინას ვჭამ და არა მტკივა."
    "Hindi: मैं काँच खा सकता हूँ, मुझे उस से कोई पीडा नहीं होती."
    "Hebrew(2): אני יכול לאכול זכוכית וזה לא מזיק לי."
    "Yiddish(2): איך קען עסן גלאָז און עס טוט מיר נישט װײ."
    "Arabic(2): أنا قادر على أكل الزجاج و هذا لا يؤلمني."
    "Japanese: 私はガラスを食べられます。それは私を傷つけません。"
    "Thai: ฉันกินกระจกได้ แต่มันไม่ทำให้ฉันเจ็บ "
  ];

  testDecoding = it "checks decoding of UTF-8 strings against Rust's String"
    (builtins.map
      (s: assertEq "Decoding of “${s}” is correct" (utf8.decode s) (rustDecode s))
      (lib.flatten [
        glassSentences
        randomUnicode
      ]));

in
  runNintTestsuite "nix.utf8" [
    testFailures
    testAscii
    testDecoding
  ]

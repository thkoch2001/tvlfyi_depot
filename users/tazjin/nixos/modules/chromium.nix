# Configure the Chromium browser with various useful things.
{ pkgs, ... }:

{
  environment.systemPackages = [
    (pkgs.chromium.override {
      enableWideVine = true; # DRM support (for Кинопоиск)
    })
  ];

  programs.chromium = {
    enable = true;
    homepageLocation = "about:blank";

    extensions = [
      "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
      "mohaicophfnifehkkkdbcejkflmgfkof" # nitter redirect
      "lhdifindchogekmjooeiolmjdlheilae" # Huruf
    ];

    extraOpts = {
      SpellcheckEnabled = true;
      SpellcheckLanguage = [
        "ru"
        "en-GB"
      ];
    };
  };
}

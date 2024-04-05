let
  note = date: description: {
    class = "note";
    inherit description date;
  };
in
[
  {
    class = "project";
    title = "VolgaSprint - Nix hacking in Kazan";
    url = "https://volgasprint.org/";
    date = 1712307024;
    description = ''
      Hacking on Nix projects for a week in Kazan, Russia, in August
      2024. Come join us!
    '';
  }
  {
    class = "misc";
    title = "@tazlog on Telegram";
    url = "https://t.me/tazlog";
    date = 1643321164;
    description = ''
      My Telegram channel with occasional random life updates and musings.
    '';
  }
  {
    class = "project";
    title = "Ship It! #37";
    url = "https://changelog.com/shipit/37";
    date = 1641819600;
    description = ''
      Podcast episode about TVL, Nix, monorepos and all sorts of related things.
    '';
  }
  {
    class = "project";
    title = "Tvix";
    url = "https://tvl.fyi/blog/rewriting-nix";
    date = 1638381387;
    description = "TVL is rewriting Nix with funding from NLNet.";
  }
  {
    class = "misc";
    title = "Interview with Joscha Bach";
    url = "https://www.youtube.com/watch?v=P-2P3MSZrBM";
    date = 1594594800;
    description = ''
      Mind-bending discussion with philosopher Joscha Bach.
    '';
  }
  {
    class = "misc";
    title = "The Virus Lounge";
    url = "https://tvl.fyi";
    date = 1587435629;
    description = "A community around Nix, monorepos, build tooling and more!";
  }
  {
    class = "project";
    title = "depot";
    url = "https://code.tvl.fyi/about";
    date = 1576800000;
    description = "Merging all of my projects into a single, Nix-based monorepo";
  }
  {
    class = "project";
    title = "Nixery";
    url = "https://github.com/google/nixery";
    date = 1565132400;
    description = "A Nix-backed container registry that builds container images on demand";
  }
  {
    class = "project";
    title = "kontemplate";
    url = "https://code.tvl.fyi/about/ops/kontemplate";
    date = 1486550940;
    description = "Simple file templating tool built for Kubernetes resources";
  }
  {
    class = "misc";
    title = "dottime";
    url = "https://dotti.me/";
    date = 1560898800;
    description = "A universal convention for conveying time";
  }
  {
    class = "project";
    title = "journaldriver";
    url = "https://code.tvl.fyi/about/ops/journaldriver";
    date = 1527375600;
    description = "Small daemon to forward logs from journald to Stackdriver Logging";
  }
  {
    class = "misc";
    title = "Principia Discordia";
    url = "https://principiadiscordia.com/book/1.php";
    date = 1495494000;
    description = ''
      A short book about everything that everyone should read.
    '';
  }
  {
    class = "misc";
    title = "Nix — не только пакетный менеджер";
    date = 1663923600;
    url = "https://www.youtube.com/watch?v=0Lhahzs-Wos";
    description = "Двухчасовой (!) разговор с введением в Nix, NixOS и так далее";
  }
  {
    class = "project";
    title = "yandex-cloud-rs";
    date = 1650877200;
    url = "https://docs.rs/yandex-cloud";
    description = "Простой SDK на Rust для работы с API Yandex Cloud.";
  }
  {
    class = "project";
    title = "nix-1p";
    date = 1564650000;
    url = "https://code.tvl.fyi/about/nix/nix-1p";
    description = "A (more or less) one-page introduction to the Nix language.";
  }
  {
    class = "misc";
    title = "Ставим NixOS!";
    date = 1678784400;
    url = "https://progmsk.timepad.ru/event/2358560/";
    description = "Встреча в undef.space для помощи в начале работы с Nix/NixOS";
  }
  {
    class = "misc";
    title = "Tvix - September '22";
    date = 1662973200;
    url = "https://tvl.fyi/blog/tvix-status-september-22";
    description = "Tvix update blog post over on TVL";
  }
  {
    class = "project";
    title = "Tvixbolt";
    date = 1667293200;
    url = "https://bolt.tvix.dev/";
    description = "In-browser language evaluator for Nix, based on Tvix";
  }
  {
    class = "project";
    title = "ООО ТВЛ";
    date = 1609491600;
    url = "https://tvl.su/ru/";
    description = "Официальный сайт моей компании по IT-консалтингу.";
  }

  # Notes.
  (note 1676106000 "If you have a Huawei device that sometimes struggles on public Wi-Fi networks, try enabling MAC-address randomisation. Huawei devices often get pushed onto management networks!")
  (note 1686868637 "I moved some of my pages (including this one) to a machine in my flat in Moscow. If you end up having access trouble because your ISP blocks Russian resources, please let me know.")
  (note 1686868636 "Protip: Use the Reddit blackout to click the 'Logout' button, and never come back.")
  (note 1486550941 "↓ I no longer recommend people to use this. Generate your configuration from a language like Nix instead.")
  (note 1576800001 "↓ No longer just my projects, it's all of TVL! Go check it out.")
]

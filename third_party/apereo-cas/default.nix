{ pkgs, ... }:

let
  name = "apereo-cas-${version}";
  version = "6.2.0";
  overlay = ./overlay;

  jdk = pkgs.jdk11;
  gradle = pkgs.gradle_6;

  deps = pkgs.stdenvNoCC.mkDerivation {
    name = "${name}-deps";
    src = overlay;
    nativeBuildInputs = with pkgs; [ gradle perl ];

    buildPhase = ''
      export GRADLE_USER_HOME="$(mktemp -d)"
      gradle --no-daemon build -x test downloadShell
    '';

    # perl code mavenizes paths (see pkgs/servers/ma1sd from nixpkgs)
    installPhase = ''
      find $GRADLE_USER_HOME/caches/modules-2 -type f -regex '.*\.\([wj]ar\|pom\)' \
        | perl -pe 's#(.*/([^/]+)/([^/]+)/([^/]+)/[0-9a-f]{30,40}/([^/\s]+))$# $src = $1; ($x = $2) =~ tr|\.|/|; $dir1 = $3; $dir2 = $4; ($y = $5) =~ s|-jvm||n; "install -Dm444 $src \$out/$x/$dir1/$dir2/$y" #e' \
        | sh
      install -m444 build/libs/cas-server-support-shell-* $out/cas-server-shell.jar
    '';

    dontStrip = true;

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "0lggx83g0nl5xwxgadgjrz62ch5qkwcyxwrd1zq8y1r7fir3m8pq";
  };
in
pkgs.stdenvNoCC.mkDerivation {
  inherit name version;

  src = overlay;
  nativeBuildInputs = with pkgs; [ gradle makeWrapper ];
  buildInputs = with pkgs; [ jdk ];

  buildPhase = ''
    export GRADLE_USER_HOME="$(mktemp -d)"

    gradle -PcacheLocation="${deps}" --offline --no-daemon build -x test
  '';

  installPhase = ''
    install -D build/libs/cas.war $out/lib/cas.war
    cp ${deps}/cas-server-shell.jar $out/lib/cas-server-shell.jar
    cp -R etc $out/etc
    makeWrapper ${jdk}/bin/java $out/bin/cas --add-flags "-Dcas.standalone.configurationDirectory=$out/etc/cas/config -jar $out/lib/cas.war"
    makeWrapper ${jdk}/bin/java $out/bin/cas-shell --add-flags "-Dcas.standalone.configurationDirectory=$out/etc/cas/config -jar $out/lib/cas-server-shell.jar"
  '';
}

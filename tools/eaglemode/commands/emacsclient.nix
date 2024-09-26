{ depot, pkgs, ... }:

let
  em = depot.tools.eaglemode;
  icon = em.mkTGA "emacs" "${pkgs.emacs}/share/icons/hicolor/128x128/apps/emacs.png";
in
em.mkCommand {
  name = "Emacsclient";
  hotkey = "Ctrl+E";
  icon = "${icon}";

  description = ''
    Open target in Emacsclient.

    Emacs server must be running already for this to have any effect.
  '';

  code = ''
    ErrorIfNotSingleTarget();

    my @tgt=GetTgt();
    my $dir=$tgt[0];

    ExecOrError('${pkgs.emacs}/bin/emacsclient', '-n', $dir);
  '';
}

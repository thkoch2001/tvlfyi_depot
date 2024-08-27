{ depot, pkgs, ... }:

depot.tools.eaglemode.mkCommand {
  name = "Emacsclient";
  hotkey = "Ctrl+E";

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

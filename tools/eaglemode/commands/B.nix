{ depot, pkgs, ... }:

let
  em = depot.tools.eaglemode;
in
em.mkCommand {
  name = "9 B";
  hotkey = "Ctrl+E";
  icon = "${./plan9.tga}";

  description = ''
    Plumb target to Sam or Acme 
  '';

  code = ''
    ErrorIfNotSingleTarget();

    my @tgt=GetTgt();
    my $dir=$tgt[0];

    ExecOrError('${pkgs.plan9port}/bin/9', 'B', $dir);
  '';
}

{ pkgs, depot, ... }:

pkgs.emacsPackages.trivialBuild {
  pname = "vterm-mgt";
  version = "1.0.0";
  src = ./vterm-mgt.el;
  packageRequires =
    (with pkgs.emacsPackages; [
      vterm
    ]) ++
    (with depot.users.wpcarro.emacs.pkgs; [
      cycle
    ]);
  passthru.meta.ci.extraSteps.github = depot.tools.releases.filteredGitPush {
    filter = ":/users/wpcarro/emacs/pkgs/vterm-mgt";
    remote = "git@github.com:wpcarro/vterm-mgt.el.git";
    ref = "refs/heads/canon";
  };
}

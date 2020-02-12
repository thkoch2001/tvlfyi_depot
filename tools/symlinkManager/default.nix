{
  depot ? import <depot> {},
  briefcase ? import <briefcase> {},
  ...
}:

depot.buildGo.program {
  name = "symlink-mgr";
  srcs = [
    ./main.go
  ];
  deps = with briefcase.gopkgs; [
    utils
  ];
}

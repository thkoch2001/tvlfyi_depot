{
  depot ? import <depot> {},
  briefcase ? import <briefcase> {},
  ...
}:

depot.buildGo.program {
  name = "job";
  srcs = [
    ./main.go
  ];
  deps = with briefcase.gopkgs; [
    kv
    utils
  ];
}

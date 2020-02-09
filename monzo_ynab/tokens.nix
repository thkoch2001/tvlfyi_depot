{
  depot ? import <depot> {},
  briefcase ? import <briefcase> {},
  ...
}:

depot.buildGo.program {
  name = "token-server";
  srcs = [
    ./tokens.go
  ];
  deps = with briefcase.gopkgs; [
    kv
  ];
}

{ depot, ... }:

depot.buildGo.package {
  name = "utils";
  srcs = [
    ./utils.go
  ];
}

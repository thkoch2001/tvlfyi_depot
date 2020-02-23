{ depot, ... }:

depot.buildGo.package {
  name = "kv";
  srcs = [
    ./kv.go
  ];
}

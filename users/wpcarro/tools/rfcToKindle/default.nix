{ depot, ... }:

# TODO: This doesn't depend on `sendgmr` at the moment, but it should. As such,
# it's an imcomplete packaging.
depot.buildGo.program {
  name = "rfcToKindle";
  srcs = [
    ./main.go
  ];
  deps = [];
}

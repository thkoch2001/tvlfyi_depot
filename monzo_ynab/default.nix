{ depot ? import <depot> {}, ... }:

depot.buildGo.program {
  name = "monzo_ynab";
  srcs = [
    ./utils.go
    ./main.go
    ./monzo.go
  ];
}

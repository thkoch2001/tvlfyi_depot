with import ./lib.nix;

let {

  l = ["1" "2" ["3" ["4"] ["5" "6"]] "7"];

  body = concat (flatten l);
}

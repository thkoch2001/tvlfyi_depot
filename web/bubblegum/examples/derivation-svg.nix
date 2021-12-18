# Warning: this is *very* slow on the first request
{ depot, ... }:

let
  inherit (depot.web.bubblegum)
    respond
    ;
in
respond "OK"
{
  Content-type = "image/svg+xml";
}
  (builtins.readFile "${depot.tvix.docs.svg}/component-flow.svg")

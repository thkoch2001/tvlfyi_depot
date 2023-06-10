version = "20230610"
author = "Emery Hemingway"
description = "Syndicated Nix Actor"
license = "Unlicense"
srcDir = "src"
bin = @["nix_actor"]
backend = "cpp"

requires "nim >= 1.6.10", "syndicate >= 20230530"

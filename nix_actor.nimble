version = "20230925"
author = "Emery Hemingway"
description = "Syndicated Nix Actor"
license = "Unlicense"
srcDir = "src"
bin = @["nix_actor"]

requires "nim >= 1.6.10", "syndicate >= 20231005", "eris >= 20230823"

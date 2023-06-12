{ depot, ... }:

let
  inherit (depot.nix) epochToDateTime;
  inherit (depot.nix.runTestsuite) assertEq it runTestsuite;
in
runTestsuite "epochToDateTimeTestsuite" [
  (it "correctly converts epoch timestamps to date-time attribute sets" [
    (assertEq "0 seconds since epoch should be 1970-01-01 00:00:00"
      { year = 1970; month = 1; day = 1; hour = 0; minute = 0; second = 0; }
      (epochToDateTime 0)
    )
    (assertEq "86461 seconds since epoch should be 1970-01-02 00:01:01"
      { year = 1970; month = 1; day = 2; hour = 0; minute = 1; second = 1; }
      (epochToDateTime 86461)
    )
    (assertEq "31536061 seconds since epoch should be 1971-01-01 00:01:01"
      { year = 1971; month = 1; day = 1; hour = 0; minute = 1; second = 1; }
      (epochToDateTime 31536061)
    )
    (assertEq "1609459261 seconds since epoch should be 2021-01-01 00:01:01"
      { year = 2021; month = 1; day = 1; hour = 0; minute = 1; second = 1; }
      (epochToDateTime 1609459261)
    )
    (assertEq "1609462861 seconds since epoch should be 2021-01-01 01:01:01"
      { year = 2021; month = 1; day = 1; hour = 1; minute = 1; second = 1; }
      (epochToDateTime 1609462861)
    )
    (assertEq "1609466461 seconds since epoch should be 2021-01-01 02:01:01"
      { year = 2021; month = 1; day = 1; hour = 2; minute = 1; second = 1; }
      (epochToDateTime 1609466461)
    )
    (assertEq "1609470061 seconds since epoch should be 2021-01-01 03:01:01"
      { year = 2021; month = 1; day = 1; hour = 3; minute = 1; second = 1; }
      (epochToDateTime 1609470061)
    )
    (assertEq "1686582396 seconds since epoch should be 2023-06-12 15:06:36"
      { year = 2023; month = 6; day = 12; hour = 15; minute = 6; second = 36; }
      (epochToDateTime 1686582396)
    )
  ])
]

# Simple datetime library for Nix.
#
# Points in time are expressed as attribute set that have the members year,
# month, day, hour, minute, second and offset (from UTC, in seconds).
# The datetime components always refer to UTC, i.e. changing an UTC time to a
# local time is as simple as `utcTime // { offset = 3600; /* UTC+1 */ }`.
#
# The library does account for leap days, but not for leap seconds. This means
# that all seconds are POSIX seconds, i.e. either 0, 1 or 2 seconds in length
# depending on leap second rules.
#
# These choices are due to implementation concerns:
#
# * It is easier to implement correct arithmetic on analyzed points in time.
# * Leap seconds don't happen on a regular basis and are only known a relatively
#   short amount of time before they happen. Additionally, unix timestamps don't
#   include extra time passed (or not passed) due to leap seconds.
#   This implies a slight correctness issue when parsing RFC3339 timestamps, as
#   ISO8601 does account for leap seconds. In practice this is rarely an issue.
# * This library does not handle time zones, so operations on localtimes couldn't
#   be implemented correctly.
#
# This library is fit for the following purposes:
#
# * Rendering and parsing RFC3339-esque datetime formats.
# * Time (only!) arithmetic.
# * Date arithmetic to a limited extent. The library does not account for time
#   zones, so the results will be correct, but using UTC as a reference point.
#   Calculating »in 6 months my timezone« with this library will return results
#   perceived as incorrect by users in a timezone with daylight saving.
#
# As indicated above, you can use this library to work with dates or times
# *only*, despite many functions always expecting a full point in time:
#
# * To work with times only, ignore the date parts. Due to the use of POSIX
#   seconds, dates don't influence time arithmetic which is then just perceived
#   to wrap around at 0:00.
# * To work with dates only, ignore the time and offset parts. Here it is
#   important to ensure that all used functions ignore time and offset, since
#   they can, in combination, cause a wrap to a different day. This also means
#   sticking to arithmetic functions that operate on days, months or years.
#
# For both the caveat about timezones and date/time arithmetic still applies.
#
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: Copyright © 2022 sterni <sternenseemann@systemli.org>
{ depot, ... }:

let
  # TODO(sterni): (rational) second fractions
  epoch = {
    year = 1970;
    month = 1;
    day = 1;
    hour = 0;
    minute = 0;
    second = 0;
    offset = 0;
  };

  inherit (depot.users.sterni.nix) int string;

  inLeapYear = { year, ... }:
    if int.mod year 400 == 0 then true
    else if int.mod year 100 == 0 then false
    else if int.mod year 4 == 0 then true
    else false;

  daysInYear = time: if inLeapYear time then 366 else 365;

  daysInMonth = { month, ... }@time:
    builtins.elemAt [
      31 # jan
      (if inLeapYear time then 29 else 28) # feb
      31 # mar
      30 # apr
      31 # may
      30 # jun
      31 # jul
      31 # aug
      30 # sep
      31 # oct
      30 # nov
      31 # dec
    ]
      (month - 1);

  secondsPerHour = 60 * secondsPerMinute;
  hoursPerDay = 24;

  secondsPerMinute = 60;
  minutesPerHour = 60;

  secondsPerDay = secondsPerHour * hoursPerDay;

  fromPosix = s:
    assert builtins.isInt s;

    let
      daysSinceEpoch = int.div s secondsPerDay;

      # TODO(sterni): This can be implemented much more efficiently without
      # recursion, but I've opted to use the variant that requires less thinking
      # for now and optimize it later.
      daysToX = name: daysInX: days: x:
        # TODO(sterni): support negative days
        assert days >= 0;
        let
          daysInThisX = daysInX { ${name} = x; };
        in

        if days < daysInThisX
        then { "${name}CarryDays" = days; ${name} = x; }
        else daysToX name daysInX (days - daysInThisX) (x + 1);

      inherit (daysToX "year" daysInYear daysSinceEpoch epoch.year)
        year
        yearCarryDays
        ;

      daysInMonth' = arg: daysInMonth ({ inherit year; inherit (arg) month; });
      inherit (daysToX "month" daysInMonth' yearCarryDays epoch.month)
        month
        monthCarryDays
        ;
    in

    {
      inherit year;
      inherit month;
      day = monthCarryDays;
      # This is very easy, since we use POSIX seconds: Every day since
      # 1970-01-01 00:00 is exactly 24h long.
      hour = int.mod (int.div s secondsPerHour) hoursPerDay;
      minute = int.mod (int.div s secondsPerMinute) minutesPerHour;
      second = int.mod s secondsPerMinute;
      # Defaults to UTC
      inherit (epoch) offset;
    };

  mkSimpleAdder =
    { name
    , carryAdder
    , max
    }:
    x: time:
    let
      sum = x + time.${name};
      carry = int.div sum max;
      newVal = int.mod sum max;
    in
    if x == 0
    then time
    else
      carryAdder carry (time // {
        ${name} = if newVal < 0 then max + newVal else newVal;
      });

  addYears = y: { year, ... }@time:
    let
      try = time // {
        year = year + y;
      };
    in
    # XXX
    if (_: true) try
    then try
    else addYears y (addDays (-1) time);

  addDays = _: time: time;

  addHours = mkSimpleAdder {
    name = "hour";
    carryAdder = addDays;
    max = hoursPerDay;
  };

  addMinutes = mkSimpleAdder {
    name = "minute";
    carryAdder = addHours;
    max = minutesPerHour;
  };

  addSeconds = mkSimpleAdder {
    name = "second";
    carryAdder = addMinutes;
    max = secondsPerMinute;
  };

  toPosix =
    { year
    , month
    , day
    , hour
    , minute
    , second
    , ...
    }:

    # TODO(sterni): support times before 1970
      assert year >= 1970;

      let
        days =
          int.sum
            (
              builtins.genList
                (y: daysInYear { year = (epoch.year + y); })
                (year - epoch.year)
              ++ builtins.genList
                (m: daysInMonth { inherit year; month = m + 1; })
                (month - epoch.month)
            )
          + day;
      in
      days * secondsPerDay
      + hour * secondsPerHour
      + minute * secondsPerMinute
      + second;

  fmtInt = width: int:
    string.fit
      {
        inherit width;
        char = "0";
      }
      (toString int);

  formatDotTime =
    { year
    , month
    , day
    , hour
    , minute
    , second
    , offset
    }:

    let
      offset' = fmtInt 2 (int.div (int.abs offset) secondsPerHour);
    in
    fmtInt 4 year
    + "-"
    + fmtInt 2 month
    + "-"
    + fmtInt 2 day
    + "T"
    + fmtInt 2 hour
    + "·"
    + fmtInt 2 minute
    + (if offset >= 0 then "+" else "-")
    + offset'
  ;

  formatRfc3339 = { offset ? null, ... }@time:
    let
      # localTime = addSeconds offset time;

      offsetAbs = int.abs offset;
      offsetHours = int.div offsetAbs secondsPerHour;
      offsetMinutes = int.div (int.mod offsetAbs secondsPerHour) secondsPerMinute;
      offsetString =
        if offset == null then "-00:00"
        else if offset == 0 then "Z"
        else (if offset > 0 then "+" else "-")
          + fmtInt 2 offsetHours
          + ":"
          + fmtInt 2 offsetMinutes;
    in

    offsetString
  ;
in

{
  inherit epoch fromPosix toPosix formatDotTime formatRfc3339;

  now = fromPosix builtins.currentTime;

  internal = {
    inherit inLeapYear daysInMonth daysInYear;
  };
}

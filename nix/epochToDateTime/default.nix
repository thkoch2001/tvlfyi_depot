# This function converts integers representing seconds since the UNIX
# epoch to an attribute set with the keys `year`, `month`, `day`,
# `hour`, `minute` and `second`.
_:

let
  # Define a modulo function
  mod = a: b: a - (a / b) * b;

  # Check if the given year is a leap year
  isLeapYear = year: mod year 4 == 0 && (mod year 100 != 0 || mod year 400 == 0);

  # Get the number of days in the given month of the given year
  daysInMonth = year: month:
    let
      monthDays = [ 31 28 31 30 31 30 31 31 30 31 30 31 ];
      febDays = if isLeapYear year then 29 else 28;
    in
    if month == 2 then febDays else builtins.elemAt monthDays (month - 1);

  epochToDateTime = epochSecs:
    let
      # Calculate the number of minutes, hours, and days from the UNIX timestamp
      epochMins = epochSecs / 60;
      epochHours = epochMins / 60;
      epochDays = epochHours / 24;

      # Calculate the date (year, month, and day) from the remaining days since the UNIX epoch
      calculateDate = days: year: month: day:
        if days == 0
        then { inherit year month day; }
        else
          let
            daysInCurrentMonth = daysInMonth year month;
          in
          if days >= daysInCurrentMonth
          then if month == 12
          then calculateDate (days - daysInCurrentMonth) (year + 1) 1 day
          else calculateDate (days - daysInCurrentMonth) year (month + 1) day
          else calculateDate 0 year month (day + days);

      # Get the date (year, month, and day) from the number of days since the UNIX epoch
      date = calculateDate epochDays 1970 1 1;

      # Calculate the time (hour, minute, and second) from the UNIX timestamp
      hour = mod epochHours 24;
      minute = mod epochMins 60;
      second = mod epochSecs 60;
    in
    {
      year = date.year;
      month = date.month;
      day = date.day;
      hour = hour;
      minute = minute;
      second = second;
    };
in
{
  __functor = _: epochToDateTime;
}

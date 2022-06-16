# horrible little module that fetches ICS files for the local trash public service.
#
# It tries its best to not overwrite existing ICS files in case the upstream goes down
# or returns empty ICS files.
import sys
import httpx
import asyncio
import icalendar
from datetime import datetime
import syslog
import os.path

# Internal id for the street (extracted from the ics download url)
ortsteil_id = "e9c32ab3-df25-4660-b88e-abda91897d7a"

# They are using a numeric encoding to refer to different kinds of trash
fraktionen = {
    "restmüll": "1",
    "bio": "5",
    "papier": "7",
    "gelbe_tonne": "13",
    "problemmüllsammlung": "20"
}

def ics_url(year):
  frakt = ','.join(fraktionen.values())
  return f'https://awido.cubefour.de/Customer/aic-fdb/KalenderICS.aspx?oid={ortsteil_id}&jahr={year}&fraktionen={frakt}&reminder=1.12:00'

def fetchers_for_years(start_year, no_of_years_in_future):
    """given a starting year, and a number of years in the future,
    return the years for which to fetch ics files"""
    current_year = datetime.now().year
    max_year = current_year + no_of_years_in_future
    return {
        "passed_years": range(start_year, current_year),
        "this_and_future_years": range(current_year, 1 + max_year)
    }

async def fetch_ics(c, url):
    """fetch an ICS file from an URL"""
    try:
        resp = await c.get(url)
    except Exception as e:
        return { "ics_does_not_exist_exc": e }

    if resp.is_error:
        return { "ics_does_not_exist": resp }
    else:
        try:
            ics = icalendar.Calendar.from_ical(resp.content)
            return { "ics": { "ics_parsed": ics, "ics_bytes": resp.content } }
        except ValueError as e:
            return { "ics_cannot_be_parsed": e }

def ics_has_events(ics):
    """Determine if there is any event in the ICS, otherwise we can assume it’s an empty file"""
    for item in ics.walk():
      if isinstance(item, icalendar.Event):
        return True
    return False

async def write_nonempty_ics(directory, year, ics):
    # only overwrite if the new ics has any events
    if ics_has_events(ics['ics_parsed']):
        path = os.path.join(directory, f"{year}.ics")
        with open(path, "wb") as f:
            f.write(ics['ics_bytes'])
            info(f"wrote ics for year {year} to file {path}")
    else:
        info(f"ics for year {year} was empty, skipping")


def main():
    ics_directory = os.getenv("ICS_DIRECTORY", None)
    if not ics_directory:
        critical("please set ICS_DIRECTORY")
    start_year = int(os.getenv("ICS_START_YEAR", 2022))
    future_years = int(os.getenv("ICS_FUTURE_YEARS", 2))

    years = fetchers_for_years(start_year, no_of_years_in_future=future_years)


    async def go():
        async with httpx.AsyncClient(follow_redirects=True) as c:
            info(f"fetching ics for passed years: {years['passed_years']}")
            for year in years["passed_years"]:
                match await fetch_ics(c, ics_url(year)):
                    case { "ics_does_not_exist_exc": error }:
                       warn(f"The ics for the year {year} is gone, error when requesting: {error} for url {ics_url(year)}")
                    case { "ics_does_not_exist": resp }:
                       warn(f"The ics for the year {year} is gone, server returned status {resp.status} for url {ics_url(year)}")
                    case { "ics_cannot_be_parsed": error }:
                       warn(f"The returned ICS could not be parsed: {error} for url {ics_url(year)}")
                    case { "ics": ics }:
                       info(f"fetched ics from {ics_url(year)}")
                       await write_nonempty_ics(ics_directory, year, ics)
                    case _:
                       critical("unknown case for ics result")


            info(f"fetching ics for current and upcoming years: {years['this_and_future_years']}")
            for year in years["this_and_future_years"]:
                match await fetch_ics(c, ics_url(year)):
                    case { "ics_does_not_exist_exc": error }:
                       critical(f"The ics for the year {year} is not available, error when requesting: {error} for url {ics_url(year)}")
                    case { "ics_does_not_exist": resp }:
                       critical(f"The ics for the year {year} is not available, server returned status {resp.status} for url {ics_url(year)}")
                    case { "ics_cannot_be_parsed": error }:
                       critical(f"The returned ICS could not be parsed: {error} for url {ics_url(year)}")
                    case { "ics": ics }:
                       info(f"fetched ics from {ics_url(year)}")
                       await write_nonempty_ics(ics_directory, year, ics)
                    case _:
                       critical("unknown case for ics result")

    asyncio.run(go())

def info(msg):
    syslog.syslog(syslog.LOG_INFO, msg)

def critical(msg):
    syslog.syslog(syslog.LOG_CRIT, msg)
    sys.exit(1)

def warn(msg):
    syslog.syslog(syslog.LOG_WARNING, msg)

def debug(msg):
    syslog.syslog(syslog.LOG_DEBUG, msg)


if __name__ == "__main__":
    main()

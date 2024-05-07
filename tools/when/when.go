package main

import (
	"fmt"
	"os"
	"strconv"
	"time"
)

const usage = `usage: when <time>

This program converts the given time into various formats (currently a local
timestamp, UTC timestamp, and UNIX epoch). It tries to guess what the input is.

Some valid queries:

  2024-01-05
  1715079241
  tomorrow 5PM
  -22h
  -7h10m

For now a single timestamp and a single duration (which is added either to the
current time, or the given time) is supported.`

func printTime(t time.Time) {
	fmt.Println("Local:", t.Format("Mon 02 January 2006 at 15:04:05 MST"))
	fmt.Println("UTC:  ", t.UTC().Format(time.RFC3339))
	fmt.Println("UNIX: ", t.Unix())
}

func setTime(this time.Time, that time.Time) time.Time {
	return time.Date(
		this.Year(),
		this.Month(),
		this.Day(),
		that.Hour(),
		that.Minute(),
		that.Second(),
		0,
		this.Location(),
	)
}

func parseTime(input string) (time.Time, error) {
	// try unix times
	if i, err := strconv.ParseInt(input, 10, 64); err == nil {
		if i < 9999999999 {
			return time.Unix(i, 0), nil
		}
		if i < 9999999999999 {
			return time.UnixMilli(i), nil
		}
	}

	// try simple date/time formats
	if t, err := time.Parse(time.DateOnly, input); err == nil {
		return t, nil
	}

	if t, err := time.Parse(time.Kitchen, input); err == nil {
		now := time.Now()
		return setTime(now, t), nil
	}

	if t, err := time.Parse(time.TimeOnly, input); err == nil {
		now := time.Now()
		return setTime(now, t), nil
	}

	if t, err := time.Parse("15:04", input); err == nil {
		now := time.Now()
		return setTime(now, t), nil
	}

	if t, err := time.Parse("3PM", input); err == nil {
		now := time.Now()
		return setTime(now, t), nil
	}

	return time.Time{}, fmt.Errorf("could not parse time: %q", input)
}

func parseDuration(input string) (time.Duration, error) {
	// some simple rewriting
	switch input {
	case "yesterday":
		input = "-24h"
	case "tomorrow":
		input = "24h"
	case "today", "now":
		return time.Duration(0), nil
	}

	// TODO: days, months, weeks, ...
	return time.ParseDuration(input)
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, usage)
		os.Exit(1)
	}

	var d time.Duration
	var t time.Time
	var err error
	var haveTime, haveDuration bool

	for _, arg := range os.Args[1:] {
		if !haveTime {
			if t, err = parseTime(arg); err == nil {
				haveTime = true
				continue
			}
		}

		if !haveDuration {
			if d, err = parseDuration(arg); err == nil {
				haveDuration = true
				continue
			}
		}
	}

	if err != nil {
		fmt.Fprintln(os.Stderr, "Not sure what you want, try another time.")
		os.Exit(1)
	}

	if haveTime && haveDuration {
		printTime(t.Add(d))
	} else if haveTime {
		printTime(t)
	} else if haveDuration {
		printTime(time.Now().Add(d))
	} else {
		fmt.Fprintln(os.Stderr, "Not sure what you want, try another time.")
		os.Exit(1)
	}
}

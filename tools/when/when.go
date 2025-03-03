package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
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
  Mar 15
  Sep 3 18:00

For now a single timestamp and a single duration (which is added either to the
current time, or the given time) is supported.`

func printTime(t time.Time) {
	fmt.Println("Local:", t.Format("Mon 02 January 2006 at 15:04:05 MST"))
	fmt.Println("UTC:  ", t.UTC().Format(time.RFC3339))
	fmt.Println("UNIX: ", t.Unix())
}

type FieldSet uint8

const (
	SetYear FieldSet = 1 << iota
	SetDay
	SetMonth
	SetHour
	SetMinute
	SetSecond
	SetLocation
)

const (
	SetDate  = SetYear | SetDay | SetMonth
	SetClock = SetHour | SetMinute | SetSecond
)

// mergeTimes returns a new time.Time with all fields in this overridden with the
// specified fields from that.
func mergeTimes(this time.Time, that time.Time, set FieldSet) time.Time {
	year, month, day := this.Date()
	hour, min, sec := this.Clock()
	loc := this.Location()

	if set&SetYear == SetYear {
		year = that.Year()
	}
	if set&SetMonth == SetMonth {
		month = that.Month()
	}
	if set&SetDay == SetDay {
		day = that.Day()
	}
	if set&SetHour == SetHour {
		hour = that.Hour()
	}
	if set&SetMinute == SetMinute {
		min = that.Minute()
	}
	if set&SetSecond == SetSecond {
		sec = that.Second()
	}
	if set&SetLocation == SetLocation {
		loc = that.Location()
	}

	return time.Date(year, month, day, hour, min, sec, 0, loc)
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
		return mergeTimes(now, t, SetClock), nil
	}

	if t, err := time.Parse(time.TimeOnly, input); err == nil {
		now := time.Now()
		return mergeTimes(now, t, SetClock), nil
	}

	if t, err := time.Parse("15:04", input); err == nil {
		now := time.Now()
		return mergeTimes(now, t, SetClock), nil
	}

	if t, err := time.Parse("3PM", input); err == nil {
		now := time.Now()
		return mergeTimes(now, t, SetClock), nil
	}

	if t, err := time.Parse(time.DateTime, input); err == nil {
		return t, nil
	}

	if t, err := time.Parse(time.Stamp, input); err == nil {
		now := time.Now()
		return mergeTimes(t, now, SetYear|SetLocation), nil
	}

	if t, err := time.Parse("Jan _2 15:04", input); err == nil {
		now := time.Now()
		return mergeTimes(t, now, SetYear|SetLocation), nil
	}

	if t, err := time.Parse("Jan _2", input); err == nil {
		now := time.Now()
		return mergeTimes(t, now, SetYear|SetLocation), nil
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

	// Try to parse entire input as one full thing, before getting more
	// clever.
	if t, err = parseTime(strings.Join(os.Args[1:], " ")); err == nil {
		printTime(t)
		return
	}

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

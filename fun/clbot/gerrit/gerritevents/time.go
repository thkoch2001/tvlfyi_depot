package gerritevents

import (
	"fmt"
	"strconv"
	"time"
)

// Time is a time.Time that is formatted as a Unix timestamp in JSON.
type Time struct {
	time.Time
}

// UnmarshalJSON unmarshals a Unix timestamp into a Time.
func (t *Time) UnmarshalJSON(bs []byte) error {
	if string(bs) == "null" {
		return nil
	}
	u, err := strconv.ParseInt(string(bs), 10, 64)
	if err != nil {
		return err
	}
	t.Time = time.Unix(u, 0)
	return nil
}

// MarshalJSON marshals a Time into a Unix timestamp.
func (t *Time) MarshalJSON() ([]byte, error) {
	if t.IsZero() {
		return []byte("null"), nil
	}
	return []byte(fmt.Sprintf("%d", t.Unix())), nil
}

// IsSet returns true if the time.Time is non-zero.
func (t *Time) IsSet() bool {
	return !t.IsZero()
}

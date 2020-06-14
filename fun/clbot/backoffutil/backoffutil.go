// Package backoffutil provides useful utilities for backoff.
package backoffutil

import (
	"time"

	backoff "github.com/cenkalti/backoff/v4"
)

// ZeroStartingBackOff is a backoff.BackOff that returns "0" as the first Duration after a reset.
// This is useful for constructing loops and just enforcing a backoff duration on every loop, rather than incorporating this logic into the loop directly.
type ZeroStartingBackOff struct {
	bo      backoff.BackOff
	initial bool
}

// NewZeroStartingBackOff creates a new ZeroStartingBackOff.
func NewZeroStartingBackOff(bo backoff.BackOff) *ZeroStartingBackOff {
	return &ZeroStartingBackOff{bo: bo, initial: true}
}

// NewDefaultBackOff creates a sensibly configured BackOff that starts at zero.
func NewDefaultBackOff() backoff.BackOff {
	ebo := backoff.NewExponentialBackOff()
	ebo.MaxElapsedTime = 0
	return NewZeroStartingBackOff(ebo)
}

// NextBackOff returns the next back off duration to use.
// For the first call after a call to Reset(), this is 0. For each subsequent duration, the underlying BackOff is consulted.
func (bo *ZeroStartingBackOff) NextBackOff() time.Duration {
	if bo.initial == true {
		bo.initial = false
		return 0
	}
	return bo.bo.NextBackOff()
}

// Reset resets to the initial state, and also passes a Reset through to the underlying BackOff.
func (bo *ZeroStartingBackOff) Reset() {
	bo.initial = true
	bo.bo.Reset()
}

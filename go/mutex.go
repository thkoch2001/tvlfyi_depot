package main

import (
	"fmt"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

func main() {
	state := make(map[int]int)
	mux := &sync.Mutex{}

	var readOps uint64
	var writeOps uint64

	// Read from state
	for i := 0; i < 1000; i += 1 {
		for j := 0; j < 100; j += 1 {
			go func() {
				key := rand.Intn(5)
				mux.Lock()
				fmt.Printf("state[%d] = %d\n", key, state[key])
				mux.Unlock()
				atomic.AddUint64(&readOps, 1)
				time.Sleep(time.Millisecond)
			}()
		}
	}

	// Write to state
	for i := 0; i < 10; i += 1 {
		for j := 0; j < 100; j += 1 {
			go func() {
				key := rand.Intn(5)
				mux.Lock()
				state[key] += 1
				mux.Unlock()
				fmt.Printf("Wrote to state[%d].\n", key)
				atomic.AddUint64(&writeOps, 1)
				time.Sleep(time.Millisecond)
			}()
		}
	}

	time.Sleep(time.Millisecond)

	mux.Lock()
	fmt.Printf("State: %v\n", state)
	mux.Unlock()
	fmt.Printf("Reads: %d\tWrites: %d\n", atomic.LoadUint64(&readOps), atomic.LoadUint64(&writeOps))
}

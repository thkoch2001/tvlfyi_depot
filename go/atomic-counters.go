// Attempting to apply some of the lessons I learned here:
// https://gobyexample.com/atomic-counters
package main

import (
	"fmt"
	"sync"
	"sync/atomic"
)

func main() {
	var count uint64
	var wg sync.WaitGroup

	for i := 0; i < 50; i += 1 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 1000; j += 1 {
				atomic.AddUint64(&count, 1)
			}
		}()
	}
	wg.Wait()
	fmt.Println("Count: ", count)
}

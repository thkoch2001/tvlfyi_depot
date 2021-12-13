package main

import (
	"fmt"
	"math/rand"
	"sync"
	"sync/atomic"
)

type readMsg struct {
	key    int
	sender chan int
}

type writeMsg struct {
	key    int
	value  int
	sender chan bool
}

func main() {
	fmt.Println("Hello, go.")

	var readOps uint64
	var writeOps uint64
	var wg sync.WaitGroup

	reads := make(chan readMsg)
	writes := make(chan writeMsg)

	go func() {
		state := make(map[int]int)
		for {
			select {
			case msg := <-reads:
				msg.sender <- state[msg.key]
			case msg := <-writes:
				state[msg.key] = msg.value
				msg.sender <- true
			}
		}
	}()

	// Reads
	for i := 0; i < 100; i += 1 {
		go func() {
			wg.Add(1)
			defer wg.Done()
			for j := 0; j < 100; j += 1 {
				msg := readMsg{
					key:    rand.Intn(5),
					sender: make(chan int)}
				reads <- msg
				val := <-msg.sender
				fmt.Printf("Received %d.\n", val)
				atomic.AddUint64(&readOps, 1)
			}
		}()
	}

	// Writes
	for i := 0; i < 100; i += 1 {
		go func() {
			wg.Add(1)
			defer wg.Done()
			for j := 0; j < 100; j += 1 {
				msg := writeMsg{
					key:    rand.Intn(5),
					value:  rand.Intn(10),
					sender: make(chan bool)}
				writes <- msg
				<-msg.sender
				fmt.Printf("Set %d as %d in state\n", msg.key, msg.value)
				atomic.AddUint64(&writeOps, 1)
			}
		}()
	}

	wg.Wait()
	fmt.Printf("Read ops: %d\tWrite ops: %d\n", atomic.LoadUint64(&readOps), atomic.LoadUint64(&writeOps))
}

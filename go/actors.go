package main

import (
	"bufio"
	"fmt"
	"os"
)

// Call function `f` in a go-routine, passing a reference to a newly created
// channel, `c`, as its only argument. Return a reference to `c` to the caller
// of `act`. When `f` halts, close the channel.
func act(f func(chan interface{})) chan interface{} {
	c := make(chan interface{})

	go func() {
		defer close(c)
		f(c)
	}()

	return c
}

func prompt(msg string) string {
	reader := bufio.NewReader(os.Stdin)
	fmt.Print(msg)
	text, _ := reader.ReadString('\n')
	// TODO: Trim trailing newline from the rhs of text.
	return text
}

func main() {
	c := act(func(c chan interface{}) {
		for {
			x := <-c
			fmt.Printf("[A] Received value: %v\n", x)

		}
	})

	for {
		x := prompt("[B] Enter a value: ")
		c <- x
	}
	os.Exit(0)
}

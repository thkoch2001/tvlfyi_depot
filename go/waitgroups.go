package main

import (
	"fmt"
	"sync"
	"time"
)

func saySomething(x string, wg *sync.WaitGroup) {
	defer wg.Done()
	fmt.Println(x)
	time.Sleep(time.Second)
	fmt.Printf("Finished saying \"%s\"\n", x)
}

func main() {
	var wg sync.WaitGroup
	var things = [5]string{"chicken", "panini", "cheeseburger", "rice", "bread"}
	for i := 0; i < 5; i += 1 {
		wg.Add(1)
		go saySomething(things[i], &wg)
	}
	wg.Wait()
}

package main

import "log"

func failOn(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

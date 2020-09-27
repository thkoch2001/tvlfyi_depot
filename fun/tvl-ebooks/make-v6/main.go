package main

import (
	"crypto/rand"
	"log"
	"net"
)

func main() {
	// 2a0c:2f07:29:9999:6564:5298:8413:4652
	ip := net.ParseIP("2a0c:2f07:29::")

	rand.Read(ip[6:])

	if ip[7] > 0xaa {
		ip[4] = 0x03
		ip[5] = 0x84
		if ip[7] > 0xdd {
			ip[4] = 0x08
			ip[5] = 0x64
		}
	}

	log.Printf("%s\n", ip)
	//
}

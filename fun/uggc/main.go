package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/pkg/browser"
)

func rot13(r rune) rune {
	if 'a' <= r && r <= 'm' {
		return r + ('n' - 'a')
	} else if 'n' <= r && r <= 'z' {
		return r - ('n' - 'a')
	}
	if 'A' <= r && r <= 'M' {
		return r + ('N' - 'A')
	} else if 'N' <= r && r <= 'Z' {
		return r - ('N' - 'A')
	}
	return r
}

func main() {
	if len(os.Args) == 0 {
		fmt.Println("usage: uggc [rot13-encoded URL]")
		return
	}
	urlText := strings.Join(os.Args[1:], " ")
	corrected := strings.Map(rot13, urlText)

	err := browser.OpenURL(corrected)

	if err != nil {
		fmt.Println("could not launch browser:", err)
	}
}

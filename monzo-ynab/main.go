// Creating a job to import Monzo transactions into YNAB.
//
// This is going to run N times per 24 hours.

package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"os/exec"
)

var (
	clientId     = os.Getenv("client_id")
	clientSecret = os.Getenv("client_secret")
)

const (
	state       = "xyz123"
	redirectUri = "http://localhost:8080/authorize"
)

func handleRedirect(w http.ResponseWriter, r *http.Request) {
	fmt.Println(r)
	fmt.Fprintf(w, "Ackified")
}

func authorizeClient() {
	url :=
		fmt.Sprintf("https://auth.monzo.com/?client_id=%s&redirect_uri=%s&response_type=code&state=:state",
			clientId, redirectUri, state)
	exec.Command("google-chrome", url).Start()
}

func main() {
	authorizeClient()
	http.HandleFunc("/authorize", handleRedirect)
	go log.Fatal(http.ListenAndServe(":8080", nil))
}

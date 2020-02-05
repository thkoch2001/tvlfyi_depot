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
	accessToken  = nil
	refreshToken = nil
)

const (
	state       = "xyz123"
	redirectUri = "http://localhost:8080/authorize"
)

func getAccessCode(string authCode) {
	form := map[string]string{
		"grant_type":    "authorization_code",
		"client_id":     client_id,
		"client_secret": client_secret,
		"redirect_uri":  redirectUri,
		"code":          authCode,
	}
	json := map[string]string{
		"access_token":  "access_token",
		"client_id":     "client_id",
		"expires_in":    21600,
		"refresh_token": "refresh_token",
		"token_type":    "Bearer",
		"user_id":       "user_id",
	}

	// TODO: Handle retry with backoff logic here.
	resp, error := http.Post("https://api.monzo.com/oauth2/token", form.Form(), json.Json())
	if err != nil {
		log.Fatal("Could not exchange authorization code for an access token.")
	}

	resp.Body()
}

func handleRedirect(w http.ResponseWriter, r *http.Request) {
	// assert that `r.state` is the same as `state`.
	params := r.URL.Query()

	reqState := params["state"][0]
	reqCode := params["code"][0]

	if reqState != state {
		log.Fatal(fmt.Sprintf("Value for state returned by Monzo does not equal our state. %s != %s", reqState, state))
	}

	go getAccessCode(reqCode)

	fmt.Printf("Received the authorization code from Monzo: %s", reqCode)
	fmt.Fprintf(w, fmt.Sprintf("Authorization code: %s", reqCode))
}

func authorizeClient() {
	url :=
		fmt.Sprintf("https://auth.monzo.com/?client_id=%s&redirect_uri=%s&response_type=code&state=%s",
			clientId, redirectUri, state)
	exec.Command("google-chrome", url).Start()
}

func main() {
	authorizeClient()
	http.HandleFunc("/authorize", handleRedirect)
	go log.Fatal(http.ListenAndServe(":8080", nil))
}

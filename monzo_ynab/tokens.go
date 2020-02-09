// Creating a Tokens server to manage my access and refresh tokens. Keeping this
// as a separate server allows me to develop and use the access tokens without
// going through client authorization.
package main

////////////////////////////////////////////////////////////////////////////////
// Dependencies
////////////////////////////////////////////////////////////////////////////////

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"net/url"
	"os"
	"time"
	"kv"
	"os/signal"
	"syscall"
)

////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////

// This is the response from Monzo's API after we request an access token
// refresh.
type refreshTokenResponse struct {
	AccessToken  string `json:"access_token"`
	RefreshToken string `json:"refresh_token"`
	ClientId     string `json:"client_id"`
	ExpiresIn    int    `json:"expires_in"`
}

// This is the shape of the request from clients wishing to set state of the
// server.
type setTokensRequest struct {
	AccessToken  string `json:"access_token"`
	RefreshToken string `json:"refresh_token"`
	ExpiresIn    int    `json:"expires_in"`
}

// This is our application state.
type state struct {
	accessToken  string `json:"access_token"`
	refreshToken string `json:"refresh_token"`
}

type readMsg struct {
	sender chan state
}

type writeMsg struct {
	state state
}

type channels struct {
	reads  chan readMsg
	writes chan writeMsg
}

////////////////////////////////////////////////////////////////////////////////
// Top-level Definitions
////////////////////////////////////////////////////////////////////////////////

var chans = &channels{
	reads:  make(chan readMsg),
	writes: make(chan writeMsg),
}

var (
	monzoClientId      = os.Getenv("monzo_client_id")
	monzoClientSecret  = os.Getenv("monzo_client_secret")
)

////////////////////////////////////////////////////////////////////////////////
// Utils
////////////////////////////////////////////////////////////////////////////////

// Schedule a token refresh for `expiresIn` seconds using the provided
// `refreshToken`. This will update the application state with the access token
// and schedule an additional token refresh for the newly acquired tokens.
func scheduleTokenRefresh(expiresIn int, refreshToken string) {
	duration := time.Second * time.Duration(expiresIn)
	timestamp := time.Now().Local().Add(duration)
	log.Printf("Scheduling token refresh for %v\n", timestamp)
	time.Sleep(duration)
	log.Println("Refreshing tokens now...")
	access, refresh := refreshTokens(refreshToken)
	log.Println("Successfully refreshed tokens.")
	chans.writes <- writeMsg{state{access, refresh}}
}

// Exchange existing credentials for a new access token and `refreshToken`. Also
// schedule the next refresh. This function returns the newly acquired access
// token and refresh token.
func refreshTokens(refreshToken string) (string, string) {
	// TODO(wpcarro): Support retries with exponential backoff.
	res, err := http.PostForm("https://api.monzo.com/oauth2/token", url.Values{
		"grant_type":    {"refresh_token"},
		"client_id":     {monzoClientId},
		"client_secret": {monzoClientSecret},
		"refresh_token": {refreshToken},
	})
	if err != nil {
		log.Println(res)
		log.Fatal("The request to Monzo to refresh our access token failed.", err)
	}
	defer res.Body.Close()
	payload := &refreshTokenResponse{}
	err = json.NewDecoder(res.Body).Decode(payload)
	if err != nil {
		log.Println(res)
		log.Fatal("Could not decode the JSON response from Monzo.", err)
	}
	go scheduleTokenRefresh(payload.ExpiresIn, payload.RefreshToken)

	return payload.AccessToken, payload.RefreshToken
}

// Listen for SIGINT and SIGTERM signals. When received, persist the access and
// refresh tokens and shutdown the server.
func handleInterrupts() {
	// Gracefully handle interruptions.
	sigs := make(chan os.Signal)
	done := make(chan bool)

	signal.Notify(sigs, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		sig := <-sigs
		log.Printf("Received signal to shutdown. %v\n", sig)
		// Persist existing tokens
		log.Println("Persisting existing credentials...")
		msg := readMsg{make(chan state)}
		chans.reads <- msg
		state := <-msg.sender
		kv.Set("monzoAccessToken", state.accessToken)
		kv.Set("monzoRefreshToken", state.refreshToken)
		log.Println("Credentials persisted.")
		done <- true
	}()

	<-done
	log.Println("Received signal to shutdown. Exiting...")
	os.Exit(0)
}

////////////////////////////////////////////////////////////////////////////////
// Main
////////////////////////////////////////////////////////////////////////////////

func main() {
	// Retrieve cached tokens from store.
	accessToken := fmt.Sprintf("%v", kv.Get("monzoAccessToken"))
	refreshToken := fmt.Sprintf("%v", kv.Get("monzoRefreshToken"))

	log.Println("Attempting to retrieve cached credentials...")
	log.Printf("Access token: %s\n", accessToken)
	log.Printf("Refresh token: %s\n", refreshToken)

	if accessToken == "" || refreshToken == "" {
		log.Fatal("Cannot start server without access or refresh tokens.")
	}

	// Gracefully shutdown.
	go handleInterrupts()

	// Manage application state.
	go func() {
		state := &state{accessToken, refreshToken}
		for {
			select {
			case msg := <-chans.reads:
				log.Printf("Reading from state.")
				log.Printf("Access Token: %s\n", state.accessToken)
				log.Printf("Refresh Token: %s\n", state.refreshToken)
				msg.sender <- *state
			case msg := <-chans.writes:
				fmt.Printf("Writing new state: %v\n", msg.state)
				*state = msg.state
			}
		}
	}()

	// Listen to inbound requests.
	fmt.Println("Listening on http://localhost:4242 ...")
	log.Fatal(http.ListenAndServe(":4242", http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		if req.URL.Path == "/refresh-tokens" && req.Method == "POST" {
			msg := readMsg{make(chan state)}
			chans.reads <- msg
			state := <-msg.sender
			go scheduleTokenRefresh(0, state.refreshToken)
			fmt.Fprintf(w, "Done.")

		} else if req.URL.Path == "/set-tokens" && req.Method == "POST" {
			// Parse
			payload := &setTokensRequest{}
			err := json.NewDecoder(req.Body).Decode(payload)
			if err != nil {
				log.Fatal("Could not decode the user's JSON request.", err)
			}

			// Update application state
			msg := writeMsg{state{payload.AccessToken, payload.RefreshToken}}
			chans.writes <- msg

			// Refresh tokens
			go scheduleTokenRefresh(payload.ExpiresIn, payload.RefreshToken)

			// Ack
			fmt.Fprintf(w, "Done.")
		} else if req.URL.Path == "/state" && req.Method == "GET" {
			// TODO(wpcarro): Ensure that this returns serialized state.
			w.Header().Set("Content-type", "application/json")
			msg := readMsg{make(chan state)}
			chans.reads <- msg
			state := <-msg.sender
			payload, _ := json.Marshal(state)
			fmt.Fprintf(w, "Application state: %s\n", bytes.NewBuffer(payload))
		} else {
			log.Printf("Unhandled request: %v\n", *req)
		}
	})))
}

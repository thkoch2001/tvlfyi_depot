// Creating a Tokens server to manage my access and refresh tokens. Keeping this
// as a separate server allows me to develop and use the access tokens without
// going through client authorization.
package main

////////////////////////////////////////////////////////////////////////////////
// Dependencies
////////////////////////////////////////////////////////////////////////////////

import (
	"auth"
	"encoding/json"
	"fmt"
	"io"
	"kv"
	"log"
	"net/http"
	"net/url"
	"os"
	"os/signal"
	"syscall"
	"time"
	"utils"
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
	state  state
	sender chan bool
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
	monzoClientId     = os.Getenv("monzo_client_id")
	monzoClientSecret = os.Getenv("monzo_client_secret")
	storePath         = os.Getenv("store_path")
)

////////////////////////////////////////////////////////////////////////////////
// Utils
////////////////////////////////////////////////////////////////////////////////

// Print the access and refresh tokens for debugging.
func logTokens(access string, refresh string) {
	log.Printf("Access: %s\n", access)
	log.Printf("Refresh: %s\n", refresh)
}

func (state *state) String() string {
	return fmt.Sprintf("state{\n\taccessToken: \"%s\",\n\trefreshToken: \"%s\"\n}\n", state.accessToken, state.refreshToken)
}

// Schedule a token refresh for `expiresIn` seconds using the provided
// `refreshToken`. This will update the application state with the access token
// and schedule an additional token refresh for the newly acquired tokens.
func scheduleTokenRefresh(expiresIn int, refreshToken string) {
	duration := time.Second * time.Duration(expiresIn)
	timestamp := time.Now().Local().Add(duration)
	// TODO(wpcarro): Consider adding a more human readable version that will
	// log the number of hours, minutes, etc. until the next refresh.
	log.Printf("Scheduling token refresh for %v\n", timestamp)
	time.Sleep(duration)
	log.Println("Refreshing tokens now...")
	accessToken, refreshToken := refreshTokens(refreshToken)
	log.Println("Successfully refreshed tokens.")
	logTokens(accessToken, refreshToken)
	setState(accessToken, refreshToken)
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
	if res.StatusCode != http.StatusOK {
		// TODO(wpcarro): Considering panicking here.
		utils.DebugResponse(res)
	}
	if err != nil {
		utils.DebugResponse(res)
		log.Fatal("The request to Monzo to refresh our access token failed.", err)
	}
	defer res.Body.Close()
	payload := &refreshTokenResponse{}
	err = json.NewDecoder(res.Body).Decode(payload)
	if err != nil {
		log.Fatal("Could not decode the JSON response from Monzo.", err)
	}

	go scheduleTokenRefresh(payload.ExpiresIn, payload.RefreshToken)

	// Interestingly, JSON decoding into the refreshTokenResponse can success
	// even if the decoder doesn't populate any of the fields in the
	// refreshTokenResponse struct. From what I read, it isn't possible to make
	// these fields as required using an annotation, so this guard must suffice
	// for now.
	if payload.AccessToken == "" || payload.RefreshToken == "" {
		log.Fatal("JSON parsed correctly but failed to populate token fields.")
	}

	return payload.AccessToken, payload.RefreshToken
}

func persistTokens(access string, refresh string) {
	log.Println("Persisting tokens...")
	kv.Set(storePath, "monzoAccessToken", access)
	kv.Set(storePath, "monzoRefreshToken", refresh)
	log.Println("Successfully persisted tokens.")
}

// Listen for SIGINT and SIGTERM signals. When received, persist the access and
// refresh tokens and shutdown the server.
func handleInterrupts() {
	// Gracefully handle interruptions.
	sigs := make(chan os.Signal, 1)
	done := make(chan bool)

	signal.Notify(sigs, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		sig := <-sigs
		log.Printf("Received signal to shutdown. %v\n", sig)
		state := getState()
		persistTokens(state.accessToken, state.refreshToken)
		done <- true
	}()

	<-done
	log.Println("Exiting...")
	os.Exit(0)
}

// Set `accessToken` and `refreshToken` on application state.
func setState(accessToken string, refreshToken string) {
	msg := writeMsg{state{accessToken, refreshToken}, make(chan bool)}
	chans.writes <- msg
	<-msg.sender
}

// Return our application state.
func getState() state {
	msg := readMsg{make(chan state)}
	chans.reads <- msg
	return <-msg.sender
}

////////////////////////////////////////////////////////////////////////////////
// Main
////////////////////////////////////////////////////////////////////////////////

func main() {
	// Manage application state.
	go func() {
		state := &state{}
		for {
			select {
			case msg := <-chans.reads:
				log.Println("Reading from state...")
				log.Println(state)
				msg.sender <- *state
			case msg := <-chans.writes:
				log.Println("Writing to state.")
				log.Printf("Old: %s\n", state)
				*state = msg.state
				log.Printf("New: %s\n", state)
				// As an attempt to maintain consistency between application
				// state and persisted state, everytime we write to the
				// application state, we will write to the store.
				persistTokens(state.accessToken, state.refreshToken)
				msg.sender <- true
			}
		}
	}()

	// Retrieve cached tokens from store.
	accessToken := fmt.Sprintf("%v", kv.Get(storePath, "monzoAccessToken"))
	refreshToken := fmt.Sprintf("%v", kv.Get(storePath, "monzoRefreshToken"))

	log.Println("Attempting to retrieve cached credentials...")
	logTokens(accessToken, refreshToken)

	if accessToken == "" || refreshToken == "" {
		log.Println("Cached credentials are absent. Authorizing client...")
		authCode := auth.GetAuthCode(monzoClientId)
		tokens := auth.GetTokensFromAuthCode(authCode, monzoClientId, monzoClientSecret)
		setState(tokens.AccessToken, tokens.RefreshToken)
		go scheduleTokenRefresh(tokens.ExpiresIn, tokens.RefreshToken)
	} else {
		setState(accessToken, refreshToken)
		// If we have tokens, they may be expiring soon. We don't know because
		// we aren't storing the expiration timestamp in the state or in the
		// store. Until we have that information, and to be safe, let's refresh
		// the tokens.
		go scheduleTokenRefresh(0, refreshToken)
	}

	// Gracefully handle shutdowns.
	go handleInterrupts()

	// Listen to inbound requests.
	fmt.Println("Listening on http://localhost:4242 ...")
	log.Fatal(http.ListenAndServe(":4242",
		http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			if req.URL.Path == "/refresh-tokens" && req.Method == "POST" {
				state := getState()
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
				setState(payload.AccessToken, payload.RefreshToken)

				// Refresh tokens
				go scheduleTokenRefresh(payload.ExpiresIn, payload.RefreshToken)

				// Ack
				fmt.Fprintf(w, "Done.")
			} else if req.URL.Path == "/state" && req.Method == "GET" {
				// TODO(wpcarro): Ensure that this returns serialized state.
				w.Header().Set("Content-type", "application/json")
				state := getState()
				payload, _ := json.Marshal(state)
				io.WriteString(w, string(payload))
			} else {
				log.Printf("Unhandled request: %v\n", *req)
			}
		})))
}

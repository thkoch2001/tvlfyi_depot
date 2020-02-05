// Exporting Monzo transactions to my YouNeedABudget.com (i.e. YNAB)
// account. YNAB unfortunately doesn't currently offer an Monzo integration. As
// a workaround and a practical excuse to learn Go, I decided to write one
// myself.
//
// This job is going to run N times per 24 hours. Monzo offers webhooks for
// reacting to certain types of events. I don't expect I'll need realtime data
// for my YNAB integration. That may change, however, so it's worth noting.

package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"net/url"
	"os"
	"os/exec"
)

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

var (
	clientId     = os.Getenv("client_id")
	clientSecret = os.Getenv("client_secret")
)

const (
	redirectURI = "http://localhost:8080/authorization-code"
	// TODO(wpcarro): Consider generating a random string for the state when the
	// application starts instead of hardcoding it here.
	state = "xyz123"
)

////////////////////////////////////////////////////////////////////////////////
// Business Logic
////////////////////////////////////////////////////////////////////////////////

// This is the response returned from Monzo when we exchange our authorization
// code for an access token. While Monzo returns additional fields, I'm only
// interested in AccessToken and RefreshToken.
type accessTokenResponse struct {
	AccessToken  string `json:"access_token"`
	RefreshToken string `json:"refresh_token"`
}

// TODO(wpcarro): Replace http.PostForm and other similar calls with
// client.postForm. The default http.Get and other methods doesn't timeout, so
// it's better to create a configured client with a value for the timeout.

func getAccessToken(code string) {
	res, err := http.PostForm("https://api.monzo.com/oauth2/token", url.Values{
		"grant_type":    {"authorization_code"},
		"client_id":     {clientId},
		"client_secret": {clientSecret},
		"redirect_uri":  {redirectURI},
		"code":          {code},
	})
	failOn(err)
	defer res.Body.Close()

	payload := accessTokenResponse{}
	json.NewDecoder(res.Body).Decode(&payload)

	log.Printf("Access token: %s\n", payload.AccessToken)
	log.Printf("Refresh token: %s\n", payload.AccessToken)
}

func listenHttp(sigint chan os.Signal) {
	// Use a go-routine to listen for interrupt signals to shutdown our HTTP
	// server.
	go func() {
		<-sigint
		// TODO(wpcarro): Do we need context here? I took this example from the
		// example on golang.org.
		log.Println("Warning: I should be shutting down and closing the connection here, but I'm not.")
		close(sigint)
	}()

	log.Fatal(http.ListenAndServe(":8080", http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		// 1. Get authorization code from Monzo.
		if req.URL.Path == "/authorization-code" {
			params := req.URL.Query()
			reqState := params["state"][0]
			code := params["code"][0]

			if reqState != state {
				log.Fatalf("Value for state returned by Monzo does not equal our state. %s != %s", reqState, state)
			}

			// TODO(wpcarro): Add a more interesting authorization confirmation
			// screen -- or even nothing at all.
			fmt.Fprintf(w, "Authorized!")

			// Exchange the authorization code for an access token.
			getAccessToken(code)
			return
		}

		log.Printf("Unhandled request: %v\n", *req)
	})))
}

// Open a web browser to allow the user to authorize this application.
// TODO(wpcarro): Prefer using an environment variable for the web browser
// instead of assuming it will be google-chrome.
func authorizeClient() {
	url := fmt.Sprintf("https://auth.monzo.com/?client_id=%s&redirect_uri=%s&response_type=code&state=%s", clientId, redirectURI, state)
	exec.Command("google-chrome", url).Start()
}

func main() {
	sigint := make(chan os.Signal, 1)
	// TODO(wpcarro): Remove state here. I'm using as a hack to prevent my
	// program from halting before I'd like it to. Once I'm more comfortable
	// using channels, this should be a trivial change.
	state := make(chan bool)

	authorizeClient()
	listenHttp(sigint)
	<-state
}

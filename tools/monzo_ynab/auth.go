package auth

////////////////////////////////////////////////////////////////////////////////
// Dependencies
////////////////////////////////////////////////////////////////////////////////

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"utils"
)

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

var (
	BROWSER      = os.Getenv("BROWSER")
	REDIRECT_URI = "http://localhost:8080/authorization-code"
)

////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////

// This is the response returned from Monzo when we exchange our authorization
// code for an access token. While Monzo returns additional fields, I'm only
// interested in AccessToken and RefreshToken.
type accessTokenResponse struct {
	AccessToken  string `json:"access_token"`
	RefreshToken string `json:"refresh_token"`
	ExpiresIn    int    `json:"expires_in"`
}

type Tokens struct {
	AccessToken  string
	RefreshToken string
	ExpiresIn    int
}

////////////////////////////////////////////////////////////////////////////////
// Functions
////////////////////////////////////////////////////////////////////////////////

// Returns the access token and refresh tokens for the Monzo API.
func GetTokensFromAuthCode(authCode string, clientID string, clientSecret string) *Tokens {
	res, err := http.PostForm("https://api.monzo.com/oauth2/token", url.Values{
		"grant_type":    {"authorization_code"},
		"client_id":     {clientID},
		"client_secret": {clientSecret},
		"redirect_uri":  {REDIRECT_URI},
		"code":          {authCode},
	})
	utils.FailOn(err)
	defer res.Body.Close()
	payload := &accessTokenResponse{}
	json.NewDecoder(res.Body).Decode(payload)

	return &Tokens{payload.AccessToken, payload.RefreshToken, payload.ExpiresIn}
}

// Open a web browser to allow the user to authorize this application. Return
// the authorization code sent from Monzo.
func GetAuthCode(clientID string) string {
	// TODO(wpcarro): Consider generating a random string for the state when the
	// application starts instead of hardcoding it here.
	state := "xyz123"
	url := fmt.Sprintf(
		"https://auth.monzo.com/?client_id=%s&redirect_uri=%s&response_type=code&state=%s",
		clientID, REDIRECT_URI, state)
	exec.Command(BROWSER, url).Start()

	authCode := make(chan string)
	go func() {
		log.Fatal(http.ListenAndServe(":8080",
			http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
				// 1. Get authorization code from Monzo.
				if req.URL.Path == "/authorization-code" {
					params := req.URL.Query()
					reqState := params["state"][0]
					code := params["code"][0]

					if reqState != state {
						log.Fatalf("Value for state returned by Monzo does not equal our state. %s != %s", reqState, state)
					}
					authCode <- code

					fmt.Fprintf(w, "Authorized!")
				} else {
					log.Printf("Unhandled request: %v\n", *req)
				}
			})))
	}()
	result := <-authCode
	return result
}

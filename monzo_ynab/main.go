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
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
	"strings"
	"os"
	"os/exec"
	"utils"
)

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

var (
	accountId    = os.Getenv("monzo_account_id")
	clientId     = os.Getenv("monzo_client_id")
	clientSecret = os.Getenv("monzo_client_secret")
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
	ExpiresIn    int    `json:"expires_in"`
}

type setTokensRequest struct {
	AccessToken  string `json:"access_token"`
	RefreshToken string `json:"refresh_token"`
	ExpiresIn    int    `json:"expires_in"`
}

type Tokens struct {
	AccessToken  string `json:"access_token"`
	RefreshToken string `json:"refresh_token"`
	ExpiresIn    int    `json:"expires_in"`
}

// TODO(wpcarro): Replace http.PostForm and other similar calls with
// client.postForm. The default http.Get and other methods doesn't timeout, so
// it's better to create a configured client with a value for the timeout.

// Returns the access token and refresh tokens for the Monzo API.
func getTokens(code string) *Tokens {
	res, err := http.PostForm("https://api.monzo.com/oauth2/token", url.Values{
		"grant_type":    {"authorization_code"},
		"client_id":     {clientId},
		"client_secret": {clientSecret},
		"redirect_uri":  {redirectURI},
		"code":          {code},
	})
	utils.FailOn(err)
	defer res.Body.Close()
	payload := &accessTokenResponse{}
	json.NewDecoder(res.Body).Decode(payload)

	return &Tokens{payload.AccessToken, payload.RefreshToken, payload.ExpiresIn}
}

// TODO(wpcarro): Prefer using an environment variable for the web browser
// instead of assuming it will be google-chrome.
// Open a web browser to allow the user to authorize this application. Return
// the authorization code sent from Monzo.
func getAuthCode() string {
	url := fmt.Sprintf("https://auth.monzo.com/?client_id=%s&redirect_uri=%s&response_type=code&state=%s", clientId, redirectURI, state)
	exec.Command("google-chrome", url).Start()

	authCode := make(chan string)
	go func() {
		log.Fatal(http.ListenAndServe(":8080", http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
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

// TODO(wpcarro): Move this logic out of here and into the tokens server.
func authorize() {
	authCode := getAuthCode()
	tokens := getTokens(authCode)
	client := &http.Client{}

	payload, _ := json.Marshal(setTokensRequest{
		tokens.AccessToken,
		tokens.RefreshToken,
		tokens.ExpiresIn})
	log.Printf("Access token: %s\n", tokens.AccessToken)
	log.Printf("Refresh token: %s\n", tokens.RefreshToken)
	log.Printf("Expires: %s\n", tokens.ExpiresIn)
	req, _ := http.NewRequest("POST", "http://localhost:4242/set-tokens", bytes.NewBuffer(payload))
	req.Header.Set("Content-Type", "application/json")
	_, err := client.Do(req)
	utils.FailOn(err)
}

// Retrieves the access token from the tokens server.
func getAccessToken() string {
	return simpleGet("http://localhost:4242/token")
}

func main() {
	accessToken := getAccessToken()
	// authHeaders := map[string]string{
	// 	"Authorization": fmt.Sprintf("Bearer %s", accessToken),
	// }

	client := &http.Client{}
	form := url.Values{"account_id": {accountId}}
	req, _ := http.NewRequest("GET", "https://api.monzo.com/transactions", strings.NewReader(form.Encode()))
	req.Header.Add("Authorization", fmt.Sprintf("Bearer %s", accessToken))
	req.Header.Add("Content-Type", "application/x-www-form-urlencoded")
	bytes, _ := httputil.DumpRequest(req, true)
	fmt.Println(string(bytes))
	res, _ := client.Do(req)
	bytes, _ = httputil.DumpResponse(res, true)
	fmt.Println(string(bytes))

	// res := simpleGet("https://api.monzo.com/accounts", authHeaders, true)
	// fmt.Println(res)

	os.Exit(0)
}

package monzoClient

import (
	"fmt"
	"log"
	"monzoSerde"
	"net/http"
	"net/url"
	"strings"
	"time"
	"tokens"
	"utils"
)

const (
	accountID = "pizza"
)

type Client struct{}

// Ensure that the token server is running and return a new instance of a Client
// struct.
func Create() *Client {
	tokens.StartServer()
	time.Sleep(time.Second * 1)
	return &Client{}
}

// Returns a slice of transactions from the last 24 hours.
func (c *Client) Transactions24Hours() []monzoSerde.Transaction {
	token := tokens.AccessToken()
	form := url.Values{"account_id": {accountID}}
	client := http.Client{}
	req, _ := http.NewRequest("POST", "https://api.monzo.com/transactions",
		strings.NewReader(form.Encode()))
	req.Header.Add("Authorization", fmt.Sprintf("Bearer %s", token))
	req.Header.Add("Content-Type", "application/x-www-form-urlencoded")
	req.Header.Add("User-Agent", "monzo-ynab")
	res, err := client.Do(req)

	utils.DebugRequest(req)
	utils.DebugResponse(res)

	if err != nil {
		utils.DebugRequest(req)
		utils.DebugResponse(res)
		log.Fatal(err)
	}
	defer res.Body.Close()

	return []monzoSerde.Transaction{}
}

// This package hosts the serialization and deserialization logic for all of the
// data types with which our application interacts from the Monzo API.
package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"time"
)

type TxMetadata struct {
	FasterPayment string `json:"faster_payment"`
	FpsPaymentId  string `json:"fps_payment_id"`
	Insertion     string `json:"insertion"`
	Notes         string `json:"notes"`
	Trn           string `json:"trn"`
}

type TxCounterparty struct {
	AccountNumber string `json:"account_number"`
	Name          string `json:"name"`
	SortCode      string `json:"sort_code"`
	UserId        string `json:"user_id"`
}

type Transaction struct {
	Id                         string    `json:"id"`
	Created                    time.Time `json:"created"`
	Description                string    `json:"description"`
	Amount                     int       `json:"amount"`
	Currency                   string    `json:"currency"`
	Notes                      string    `json:"notes"`
	Metadata                   TxMetadata
	AccountBalance             int            `json:"account_balance"`
	International              interface{}    `json:"international"`
	Category                   string         `json:"category"`
	IsLoad                     bool           `json:"is_load"`
	Settled                    time.Time      `json:"settled"`
	LocalAmount                int            `json:"local_amount"`
	LocalCurrency              string         `json:"local_currency"`
	Updated                    time.Time      `json:"updated"`
	AccountId                  string         `json:"account_id"`
	UserId                     string         `json:"user_id"`
	Counterparty               TxCounterparty `json:"counterparty"`
	Scheme                     string         `json:"scheme"`
	DedupeId                   string         `json:"dedupe_id"`
	Originator                 bool           `json:"originator"`
	IncludeInSpending          bool           `json:"include_in_spending"`
	CanBeExcludedFromBreakdown bool           `json:"can_be_excluded_from_breakdown"`
	CanBeMadeSubscription      bool           `json:"can_be_made_subscription"`
	CanSplitTheBill            bool           `json:"can_split_the_bill"`
	CanAddToTab                bool           `json:"can_add_to_tab"`
	AmountIsPending            bool           `json:"amount_is_pending"`
	// Fees interface{} `json:"fees"`
	// Merchant interface `json:"merchant"`
	// Labels interface{} `json:"labels"`
	// Attachments interface{} `json:"attachments"`
	// Categories interface{} `json:"categories"`
}

// Attempts to encode a Monzo transaction struct into a string.
func serializeTx(tx *Transaction) (string, error) {
	x, err := json.Marshal(tx)
	return string(x), err
}

// Attempts to parse a string encoding a transaction presumably sent from a
// Monzo server.
func deserializeTx(x string) (*Transaction, error) {
	target := &Transaction{}
	err := json.Unmarshal([]byte(x), target)
	return target, err
}

func main() {
	b, _ := ioutil.ReadFile("./fixture.json")
	tx := string(b)
	target, _ := deserializeTx(tx)
	out, _ := serializeTx(target)
	fmt.Println(out)
}

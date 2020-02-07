// This package hosts the serialization and deserialization logic for all of the
// data types with which our application interacts from the YNAB API.
package main

import (
	"encoding/json"
	"fmt"
	"time"
)

type Transaction struct {
	Id           string    `json:"id"`
	Date         time.Time `json:"date"`
	Amount       int       `json:"amount"`
	Memo         string    `json:"memo"`
	Cleared      string    `json:"cleared"`
	Approved     bool      `json:"approved"`
	FlagColor    string    `json:"flag_color"`
	AccountId    string    `json:"account_id"`
	AccountName  string    `json:"account_name"`
	PayeeId      string    `json:"payeed_id"`
	PayeeName    string    `json:"payee_name"`
	CategoryId   string    `json:"category_id"`
	CategoryName string    `json:"category_name"`
	Deleted      bool      `json:"deleted"`
	// TransferAccountId interface{} `json:"transfer_account_id"`
	// TransferTransactionId interface{} `json:"transfer_transaction_id"`
	// MatchedTransactionId interface{} `json:"matched_transaction_id"`
	// ImportId interface{} `json:"import_id"`
	// Subtransactions interface{} `json:"subtransactions"`
}

// Attempts to encode a YNAB transaction into a string.
func serializeTx(tx *Transaction) (string, error) {
	x, err := json.Marshal(tx)
	return string(x), err
}

// Attempts to parse a string encoding a transaction presumably sent from a
// YNAB server.
func deserializeTx(x string) (*Transaction, error) {
	target := &Transaction{}
	err := json.Unmarshal([]byte(x), target)
	return target, err
}

func main() {
	target, _ := deserializeTx(tx)
	out, _ := serializeTx(target)
	fmt.Println(out)
	fmt.Println(ynabOut)
}

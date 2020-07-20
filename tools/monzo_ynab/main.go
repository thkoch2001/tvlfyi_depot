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
	"fmt"
)

var (
	ynabAccountID = os.Getenv("ynab_account_id")
)

////////////////////////////////////////////////////////////////////////////////
// Business Logic
////////////////////////////////////////////////////////////////////////////////

// Convert a Monzo transaction struct, `tx`, into a YNAB transaction struct.
func toYnab(tx monzoSerde.Transaction) ynabSerde.Transaction {
	return ynabSerde.Transaction{
		Id: tx.Id,
		Date: tx.Created,
		Amount: tx.Amount,
		Memo: tx.Notes,
		AccountId: ynabAccountID,
	}
}

func main() {
	txs := monzo.TransactionsLast24Hours()
	var ynabTxs []ynabSerde.Transaction{}
	for tx := range txs {
		append(ynabTxs, toYnab(tx))
	}
	ynab.PostTransactions(ynabTxs)
	os.Exit(0)
}

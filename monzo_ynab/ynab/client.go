package client

import (
	"serde"
)

// See requests.txt for more details.
func PostTransactions(accountID string, txs []serde.Transaction{}) error {
	return map[string]string{
		"transactions": [
			{
				"account_id": accountID,
					"date": "2019-12-30",
					"amount": 10000,
					"payee_name": "Richard Stallman",
					"memo": "Not so free software after all...",
					"cleared": "cleared",
					"approved": true,
					"flag_color": "red",
					"import_id": "xyz-123"
			}
		]
	}
}

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

////////////////////////////////////////////////////////////////////////////////
// Business Logic
////////////////////////////////////////////////////////////////////////////////

func main() {
	fmt.Println("To be implemented...")
}

# tagtime reimplementation

What’s great about original perl tagtime?

* timestamps are deterministic from the beginning (keep)
* the tagging system should just work (tm)

What’s the problem with the original perl tagtime?

* it uses a bad, arbitrary file format -> sqlite3
* the query window does not time out, so it’s easy to miss that it’s open (often hidden behind another window), and then the following pings might never appear)
* There’s a bug with tags containing a `.` -> sqlite3

What would be cool to have?

* multi-entry mode (ping on phone and laptop and merge the replies eventually since they will apply to single timestamps)
* simplifying reporting based on fuzzy matching & history
* auto-generate nice time reports with hours for work items

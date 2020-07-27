# TopTal take-home #2

All of the commands defined herein should be run from the top-level directory of
this repository (i.e. the directory in which this file exists).

## Database

Create a new database named `db.sqlite3` with:

```shell
$ sqlite3 db.sqlite3
```

Initialize the schema with:

```
sqlite> .read src/init.sql
```

You can verify that you successfully initialized the database by running:

```
sqlite> .tables
sqlite> .schema Accounts
sqlite> .schema Trips
```

Populate the database with some dummy values using the following:

```
sqlite> PRAGMA foreign_keys = on;
sqlite> .mode csv
sqlite> .import data/accounts.csv Accounts
sqlite> .import data/trips.csv Trips
```

You can verify you successfully populated the tables with:

```
sqlite> .mode columns
sqlite> .headers on
sqlite> SELECT * FROM Accounts;
sqlite> SELECT * FROM Trips;
```

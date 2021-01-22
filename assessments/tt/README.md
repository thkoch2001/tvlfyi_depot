# TT

All of the commands defined herein should be run from the top-level directory of
this repository (i.e. the directory in which this file exists).

## Server

To create the environment that contains all of this application's dependencies,
run:

```shell
$ nix-shell
```

To run the server interactively, run:

```shell
$ cd src/
$ ghci
```

Now compile and load the server with:

```
Prelude> :l Main.hs
*Main> main
```

## Database

Create a new database named `db.sqlite3` with:

```shell
$ sqlite3 db.sqlite3
```

Populate the database with:

```
sqlite3> .read populate.sqlite3
```

You can verify that everything is setup with:

```
sqlite3> .tables
sqlite3> .schema
sqlite3> SELECT * FROM Accounts;
sqlite3> SELECT * FROM Trips;
```

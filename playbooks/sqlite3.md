# SQLite3

Creating a reference for SQLite that I can access when I'm offline
(e.g. traveling in an airplane).

## Benefits

I enjoy using SQLite because it's lightweight and simple. Instead of networking
microservices, I can oftentimes just create a simple `db.sqlite3` file and get
significant mileage without much tooling overhead.

## Limitations

SQLite has some limitations; here are some of the limitations that I have encountered.

- SQLite **disables** support for `FOREIGN KEY` by default. Enable it with:

```
sqlite> PRAGMA foreign_keys = ON;
```

- SQLite has no `BOOLEAN` type; it uses 0 and 1 instead.

```
sqlite> SELECT TRUE;
TRUE
----------
1
sqlite> SELECT FALSE;
FALSE
----------
0
```

- SQLite has no `DATETIME` type; it uses `TEXT` instead.

```
sqlite> SELECT datetime('now');
datetime('now')
-------------------
2020-07-26 09:52:32
```

## Reference

The following should serve as a useful reference for working with SQLite.

### Schema

```sql
CREATE TABLE IF NOT EXISTS Movies (
  title TEXT NOT NULL,
  year INTEGER,
  PRIMARY KEY (title)
);

ALTER TABLE Movies ADD COLUMN rating DEFAULT 0.0;

DROP TABLE Movies;
```

### Queries

The following queries should come in handy as a reference:

```
sqlite> -- I'm using an intentionally incorrect date here for the subsequent UPDATE.
sqlite> INSERT INTO Movies (title, year) VALUES ('Toy Story 3', 2100);
sqlite> SELECT * FROM Movies WHERE year IS NULL;
sqlite> UPDATE Movies SET year = 2010 WHERE title = 'Toy Story 3';
sqlite> -- % is like .* in a regex
sqlite> DELETE FROM Movies WHERE title LIKE 'Toy Story%';
```

## Command Line

- Create a `~/.sqliterc` file with the following contents:

```
.mode column
.headers on
```

- To start an interactive session:

```shell
$ sqlite3 db.sqlite3
```

- To create a SQLite database from a `.sql` file:

```shell
$ sqlite3 db.sqlite3 <db.sql
```

- To reload changes to a `.sql` file while in an interactive session:

```
sqlite> .read db.sql
```

## Miscellaneous

- For a web-browser-based SQLite viewer, run the following:

```shell
$ sqlite_web db.sqlite3
```

- To import a CSV:

```
sqlite> .mode csv <table-name>
sqlite> .import path/to/file.csv <table-name>
```

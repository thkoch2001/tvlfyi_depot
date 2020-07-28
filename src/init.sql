-- Run `.read init.sql` from within a SQLite3 REPL to initialize the tables we
-- need for this application. This will erase all current entries, so use with
-- caution.
-- Make sure to set `PRAGMA foreign_keys = on;` when transacting with the
-- database.

BEGIN TRANSACTION;

DROP TABLE IF EXISTS Accounts;
DROP TABLE IF EXISTS Trips;
DROP TABLE IF EXISTS Sessions;

CREATE TABLE Accounts (
-- TODO(wpcarro): Add CHECK(..) constraint
  username TEXT CHECK(LENGTH(username) > 0) NOT NULL,
  password TEXT CHECK(LENGTH(password) > 0) NOT NULL,
  email TEXT CHECK(LENGTH(email) > 0) NOT NULL UNIQUE,
  role TEXT CHECK(role IN ('user', 'manager', 'admin')) NOT NULL,
  profilePicture BLOB,
  PRIMARY KEY (username)
);

CREATE TABLE Trips (
  username TEXT NOT NULL,
  destination TEXT CHECK(LENGTH(destination) > 0) NOT NULL,
  startDate TEXT CHECK(LENGTH(startDate) == 10) NOT NULL, -- 'YYYY-MM-DD'
  endDate TEXT CHECK(LENGTH(endDate) == 10) NOT NULL, -- 'YYYY-MM-DD'
  comment TEXT NOT NULL,
  PRIMARY KEY (username, destination, startDate),
  FOREIGN KEY (username) REFERENCES Accounts ON DELETE CASCADE
);

CREATE TABLE Sessions (
  uuid TEXT CHECK(LENGTH(uuid) == 36) NOT NULL,
  username TEXT NOT NULL UNIQUE,
  tsCreated TEXT CHECK(LENGTH(tsCreated) == 33) NOT NULL, -- 'YYYY-MM-DD HH:MM:SS'
  PRIMARY KEY (uuid),
  FOREIGN KEY (username) REFERENCES Accounts ON DELETE CASCADE
);

COMMIT;

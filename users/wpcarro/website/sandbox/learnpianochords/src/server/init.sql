BEGIN TRANSACTION;

DROP TABLE IF EXISTS GoogleLinkedAccounts;
DROP TABLE IF EXISTS PayingCustomers;
DROP TABLE IF EXISTS Sessions;

-- Store some of the information that Google provides to us from the JWT.
CREATE TABLE GoogleLinkedAccounts (
  accountUUID TEXT CHECK(LENGTH(uuid) == 36) NOT NULL UNIQUE,
  email TEXT NOT NULL UNIQUE,
  tsCreated TEXT NOT NULL, -- 'YYYY-MM-DD HH:MM:SS'
  givenName TEXT,
  familyName TEXT,
  fullName TEXT,
  pictureURL TEXT,
  locale TEXT,
  PRIMARY KEY (accountUUID)
);

-- Track which of our customers have a paid account.
-- Defines a one-to-one relationship between:
--   GoogleLinkedAccounts and PayingCustomers
CREATE TABLE PayingCustomers (
  accountUUID TEXT,
  tsCreated TEXT,
  PRIMARY KEY (accountUUID),
  FOREIGN KEY (accountUUID) REFERENCES GoogleLinkedAccounts ON DELETE CASCADE
);

-- Define mobile and web sessions for our users.
-- Defines a one-to-many relationship between:
--   GoogleLinkedAccounts and Sessions
CREATE TABLE Sessions (
  sessionUUID TEXT CHECK(LENGTH(sessionUUID) == 36) NOT NULL UNIQUE,
  accountUUID TEXT,
  tsCreated TEXT NOT NULL, -- 'YYYY-MM-DD HH:MM:SS'
  PRIMARY KEY (sessionUUID)
  FOREIGN KEY(accountUUID) REFERENCES GoogleLinkedAccounts ON DELETE CASCADE
);

COMMIT;

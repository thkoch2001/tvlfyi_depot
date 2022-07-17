-- SQLite
.dump

PRAGMA foreign_keys = ON;

BEGIN transaction;

create table if not exists file_content (
  content blob NOT NULL,
  hash_sha256 blob PRIMARY KEY,
  size integer NOT NULL
) WITHOUT ROWID;


create table if not exists file_references (
  rowid integer PRIMARY KEY,
  file_content NOT NULL REFERENCES file_content ON DELETE CASCADE,
  reference_type text NOT NULL,
  name text NOT NULL,
  extension text NOT NULL,
  mimetype text NOT NULL
);

create unique index if not exists file_references_type_name_unique on file_references (reference_type, name);

-- insert into file_content values ('mycontent', 'myhash', 9);
-- insert into file_references values (NULL, 'myhash', 'by-id', 'myschranz', '.txt', 'text/plain');
-- insert into file_content values (readfile('/home/philip/Pictures/screenshot.png'), 'anotherhash', 999);
-- insert into file_references values (NULL, 'anotherhash', 'by-id', 'img', '.png', 'image/png');

select * from file_content;

select * from file_references;

COMMIT;

-- drop table file_content;
-- drop table file_references;

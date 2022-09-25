* pl/nix procedural language

** Why

- To my experience usually the best way of working with databases is
  through predefined list of functions procedures that have expected
  return type. They define "the contract" between application server
  and database and it's the easiest way to have good test coverage for
  database related operations. Currently PostgreSQL offers already an
  extensive list of pl-languages:
    1. sql (default declarative)
    2. pl/pgsql (most popular procedural)
    3. pl/python (usually for pushing down scientific work)
    4. pl/tcl
    5. pl/perl
    6. pl/rust

- Traversing attribute sets in PostgreSQL is hard. PostgreSQL has
  native json/jsonb support. But working with it inside of the
  database is painful. I believe that nix is a better language, it
  could be used for ad-hoc in-database search or writing helper
  functions. Here's an example of current syntax, that I find hard to
  reason about:

    ```sql
    SELECT preferences->'beta'
    FROM users
    WHERE (preferences->>'beta')::boolean IS TRUE;
    ```

- Writing recursive logic is another painful point of
  PostgreSQL. Cursors are a terrible abstraction, having a language
  with higher order functions and tail optimization might be
  beneficial for writing anything that requires folds.

  Here is an example of an iterator written in pl/pgsql:

    ```sql
    CREATE OR REPLACE FUNCTION get_film_titles(p_year integer)
       RETURNS text AS $$
    DECLARE
         titles text default '';
         rec_film   record;
         cur_films cursor(p_year integer)
             for select title, release_year
             from film
             where release_year = p_year;
    BEGIN
       -- open the cursor
       OPEN cur_films(p_year);

       LOOP
        -- fetch row into the film
          FETCH cur_films INTO rec_film;
        -- exit when no more row to fetch
          EXIT WHEN NOT FOUND;

        -- build the output
          IF rec_film.title LIKE '%ful%' THEN
             titles := titles || ',' || rec_film.title || ':' || rec_film.release_year;
          END IF;
       END LOOP;

       -- close the cursor
       CLOSE cur_films;

       RETURN titles;
    END; $$
    ```

** Examples of use syntax

*** Registration
```sql
CREATE EXTENSION IF NOT EXISTS plnix;
```

*** Primitives
```sql
CREATE FUNCTION get_string() RETURNS TEXT
    IMMUTABLE STRICT
    LANGUAGE PLNIX AS
$$
    "hello world"
$$;
SELECT get_string();
/*
get_string()
----------------
   "hello world"
(1 row)
```

*** Arrays
```sql
CREATE FUNCTION sum_array(_a BIGINT[]) RETURNS BIGINT
    IMMUTABLE STRICT
    LANGUAGE PLNIX AS
$$
    builtins.foldl' (x: acc: x + acc) 0 _a
$$;
SELECT sum_array(ARRAY[1,2,3]);
/*
sum_array
----------------
              6
(1 row)
*/
```

*** Attrsets
```sql
CREATE TABLE test_table (
    name text,
    value jsonb
);
INSERT INTO test_table VALUES (
    'test',
    '{"a":{"b":"c"}}'
);
CREATE FUNCTION transform(_a jsonb) RETURNS jsonb
    IMMUTABLE STRICT
    LANGUAGE PLNIX AS
$$
    _a // { _a.a = { c = "b"; }; }
$$;
SELECT name, transform(value) FROM test_table;
/*
name         |            value
-------------------------------
    "test"   |  a = { c = "b" }
(1 row)
```

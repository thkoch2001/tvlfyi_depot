# Simple Select

- Simple Select is a less expressive but more ergonomic query language for
  tabular data than SQL.
- `slx` is a command-line tool for querying CSVs using the Simple Select query
  language.

Simple Select queries look like this: `director:"Tarantino" OR director:"Scorsese"`.

## Example

Say we have the following data in a CSV:

```csv
title,year,rating,director
"Spirited Away",2001,8.5,"Hayao Miyazaki"
Andhadhun,2018,8.1,"Sriram Raghavan"
Dangal,2016,8.3,"Sriram Raghavan"
"Avengers: Infinity War",2019,8.4,"Anthony Russo"
Alien,1979,8.4,"Ridley Scott"
...
```

We can invoke `slx` like so...

```
$ slx -f /tmp/movies.csv
```

...and then query using the REPL:

```
> director:/S.*m/ OR director:"Hayao"
Andhadhun       2018    8.1     1       Sriram Raghavan 0       1
Dangal  2016    8.3     1       Sriram Raghavan 0       1
Howls Moving Castle     2004    8.2     0       Hayao Miyazaki  1       1
Judgment at Nuremberg   1961    8.1     0       Stanley Kramer  0       0
Laputa: Castle in the Sky       1986    8.0     0       Hayao Miyazaki  1       1
Nausicaa of the Valley of the Wind      1984    8.0     0       Hayao Miyazaki  1       1
Network 1976    8.1     0       Sidney Lumet    0       0
```

## Warning

Simple Select is **not intended for production use**. I wrote this as a toy
project for my own consumption. There are quite a few bugs of which I'm aware
and quite a few other features that I'd like to support but haven't had time to
support just yet.

Why publish it then? Maybe this project will inspire drive-by contributions or
other, better-implemented spin-offs.

## Wish List

Speaking of drive-by contributions, here are some things that I'd like to
support:

- Implicit `AND` conjunctions (`director:/Tarantino/ year:"2000"` instead of
  `director:/Tarantino/ AND year:"2000"`)
- Support for types like numbers, dates (`year:2000` instead of `year:"2000"`)
- `slx` should support CSV *and* (at the very least) sqlite3 file formats (open
  to other formats as well)
- Regexes should be the default query primitive (`director:Tarantino` instead of
  `director:/Tarantino/`)
- Improve parsing errors (including surfacing errors to the user)
- Support for reading from `STDIN` and issuing queries from the command-line
- Unit-testing
- Configurable delimiters for output data (right now it's just `\t`)
- (Maybe) rewrite in a faster, more-type-safe languages (e.g. Rust)

I'm likely missing other FRs, bugs, so please file issues!

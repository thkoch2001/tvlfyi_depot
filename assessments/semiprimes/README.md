# Semiprimes Service

## Introduction

A **composite** is a number containing at least two prime factors. For example:

```
15 = 3 × 5
9 = 3 × 3
12 = 2 × 2 × 3
```

There are ten composites below thirty containing precisely two, not necessarily
distinct, prime factors: `4, 6, 9, 10, 14, 15, 21, 22, 25, 26`. Let’s call such
numbers *Semiprimes*.

## Task

- Write a module which provides a function to tell whether a given number, `N`,
  is a semiprime. `N` will be less than 100,000
- Please implement an API (RESTful or GraphQL) to factor a given number into two
  prime numbers if it’s a semiprime, otherwise, return an error message.

## Stretch Goals

- Handle the invalid inputs.
- Support batch requests: i.e. users could provide 100 numbers, and the API
  return the answer for all.
- Considering this module will be used by a long running service, could you
  optimize it to give answers faster?

## Usage

To run the application you'll need to have `elixir` installed. Assuming `elixir`
is already installed, consult the following steps to start the application:

```shell
$ cd server
$ mix deps.get
$ iex -S mix
```

Now open a web browser and visit `http://localhost:8080`!


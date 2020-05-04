# Clojure Boilerplate

This boilerplate uses `lein` to manage the project.

## Files to change

To use this boilerplate, run the following in a shell:

```shell
$ cp ~/briefcase/boilerplate/clojure path/to/new-project
```

After running the above command, change the following files to remove the
placeholder values:

- `README.md`: Change the title; change the description; drop "Files to change";
  keep "Getting started"
- `project.clj`: Change title
- `src/project.clj`: Change `:doc`; drop `main/foo`

## Getting started

From a shell, run:

```shell
$ lein repl
```

From Emacs, navigate to a source code buffer and run:

```
M-x cider-jack-in
```

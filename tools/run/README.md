# run

Simplify the commands you call to run scripts on the command line.

```shell
> run path/to/file.py
> run path/to/file.ts
```

## How?

Define a run.json configuration mapping commands to filename extensions like
so:
```json
{
  ".ts": "npx ts-node $file",
  ".py": "python3 $file"
}
```

Then call `run path/to/some/file.ts` on the command line, and `npx ts-node
file.ts` will run.

## Installation

Install `run` using Nix.

```shell
> nix-env -iA briefcase.run
```

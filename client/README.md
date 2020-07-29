# Elm

Elm has one of the best developer experiences that I'm aware of. The error
messages are helpful and the entire experience is optimized to improve the ease
of writing web applications.

## Developing

If you're interested in contributing, the following will create an environment
in which you can develop:

```shell
$ nix-shell
$ elm-live -- src/Main.elm --output=Main.min.js
```

You can now view your web client at `http://localhost:8000`!

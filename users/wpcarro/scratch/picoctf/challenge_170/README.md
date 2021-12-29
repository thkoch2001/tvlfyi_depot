# challenge 170

The following should work on most Linux distros, but it didn't for me on NixOS:

```shell
chmod u+x ./warm
./warm -h
```

So instead, just call `strings` on the exectuable to search for the help text,
which contains the flag.

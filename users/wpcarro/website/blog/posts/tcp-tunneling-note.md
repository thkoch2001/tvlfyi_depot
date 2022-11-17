## Background

Let's say we'd like to debug a remote machine but use some of the debugging
tools we have on our local machine like wireshark.

You *can* run `tcpdump` on the remote and then `scp` the file to your local
machine to analyze the traffic, but after doing that a few times you may want a
workflow with a tighter feedback loop. For this we'll forward traffic from a
remote machine to our local machine.

**Note:** There's also `termshark`, which is a `wireshark` TUI that you can run
on the remote. It's quite cool!

## Local

Run the following on your local machine to forward your remote's traffic:

```shell
$ ssh -R 4317:127.0.0.1:4317 -N -f user@remote
```

Here is an abridged explanation of the flags we're passing from `man ssh`:

```
-N     Do  not  execute  a remote command.  This is useful for just forwarding ports.
-f     Requests ssh to go to background just before command execution.
```

**Note:** I couldn't find a good explanation for the `-R` option, so I tried
removing it and re-running the command, but that results in a resolution error:

```
ssh: Could not resolve hostname 4317:127.0.0.1:4317: Name or service not known
```

The remote should now be forwarding traffic from port `4317` to our
machine. We can verify with the following:

```shell
$ nc -l 4317 -k
```

## Testing

Let's generate some traffic on the remote. **Note:** you should see the output
in the shell in which you're running `nc -l 4317 -k`.

```shell
$ telnet localhost 4317
Trying ::1...
Connected to localhost.
Escape character is '^]'.
hello
world
```

Locally you should see:

```shell
λ nc -l 4317 -k
hello
world
```

You should now be able to `tcpdump -i lo port 4317` or just use `wireshark`
locally.

Happy debugging!

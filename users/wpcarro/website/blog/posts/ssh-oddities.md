## Background

I was trying to debug a service over `ssh` that offered password-only
authentication, but I couldn't seem to get the `ssh` client to prompt me for the
password.

It looked something like this (skip ahead to the conclusion if you're pressed
for time):

## Troubleshooting

```shell
λ ssh root@[redacted]
Unable to negotiate with [redacted] port 22: no matching host key type found. Their offer: ssh-rsa
```

But the same command was working just fine for my coworker.

I took a closer look with `ssh -v root@[redacted]`, but nothing jumped-out at
me. Maybe it's something with *my* `ssh` configuration; let's remove that
variable:

```shell
λ ssh -F /dev/null root@[redacted]
Unable to negotiate with [redacted] port 22: no matching host key type found. Their offer: ssh-rsa
```

> Ah it looks like there's a way to set my preferred authentication method...
> -- me

```shell
λ ssh -F /dev/null -o PreferredAuthentications=password root@[redacted]
Unable to negotiate with [redacted] port 22: no matching host key type found. Their offer: ssh-rsa
```

## Conclusion

Well it turns-out that newer SSH clients disable the `ssh-rsa` public key
signature algorithm because it depends on SHA-1, which is considered insecure.

```shell
λ ssh -V
OpenSSH_9.0p1, OpenSSL 1.1.1p  21 Jun 2022
```

...and according to the `ssh -v` output, the server is running a pre-COVID(!!!)
version of `ssh`:

```
debug1: Remote protocol version 2.0, remote software version dropbear_2018.76
```

So if you don't have time to upgrade the SSH server, and you just want to
connect, the following should work because we're *opting-into* the less secure
option:

```shell
λ ssh -o HostKeyAlgorithms=+ssh-rsa root@[redacted]
```

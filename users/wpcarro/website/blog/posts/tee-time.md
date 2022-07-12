I encountered this fun TIL while troubleshooting Linux write permissions
issues...

## TL;DR

Don't do this (unless you want misleading test results):

```shell
λ sudo -u node-exporter echo 'Hello, world' >/var/lib/textfile-exporter/test.prom
```

Do this:

```shell
λ echo 'Hello, world' | sudo -u node-exporter tee /var/lib/textfile-exporter/test.prom
```

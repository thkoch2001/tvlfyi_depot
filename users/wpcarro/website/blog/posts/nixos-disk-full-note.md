## Background

Every now and then NixOS hosts runs out of disk space. This happened to my IRC
server recently...

> No problem. Let's free-up some space with Nix's garbage-collection:
> - me

```shell
λ nix-collect-garbage -d # failed due lack of disk space
```

Ironically Nix needs to do an SQLite transaction before deleting stuff and
SQLite can't do that if there's no space. This is especially funny because the
SQLite is probably a `DELETE`.

## Solution

First let's verify that our disk is indeed at capacity:

```shell
λ df -h
Filesystem                Size  Used Avail Use% Mounted on
devtmpfs                  399M     0  399M   0% /dev
tmpfs                     3.9G     0  3.9G   0% /dev/shm
tmpfs                     2.0G  3.7M  2.0G   1% /run
tmpfs                     3.9G  408K  3.9G   1% /run/wrappers
/dev/disk/by-label/nixos  9.9G  9.9G    0G 100% /
tmpfs                     4.0M     0  4.0M   0% /sys/fs/cgroup
tmpfs                     797M     0  797M   0% /run/user/0
```

Looks like `/dev/disk/by-label/nixos` is at `100%`. Now let's find some easy
targets to free-up space so that we can run `nix-collect-garbage -d`...

```shell
λ du -hs /* 2>/dev/null
8.0K    /bin
12M     /boot
0       /dev
200K    /etc
68K     /home
16K     /lost+found
9.0G    /nix
0       /proc
1.2M    /root
2.9M    /run
4.0K    /srv
0       /sys
44K     /tmp
12K     /usr
1.2G    /var
```

Okay: `/var` looks like an easy candidate. Let's recurse into that directory:

```shell
λ du -hs /var/*
40K     /var/cache
12K     /var/db
4.0K    /var/empty
4.0K    /var/google-users.d
211M    /var/lib
0       /var/lock
918M    /var/log
0       /var/run
4.0K    /var/spool
44K     /var/tmp
λ du -hs /var/log/* # /var/log looks promising
60M     /var/log/btmp
82M     /var/log/btmp.1
776M    /var/log/journal # ah-ha! journald. Let's clean-up some logs
8.0K    /var/log/lastlog
1.1M    /var/log/nginx
4.0K    /var/log/private
12K     /var/log/wtmp
```

To retain at most 1w's worth of logs:

```shell
λ journalctl --vacuum-time=1w
```

...or if you'd prefer to retain only 100M's worth of logs:

```shell
λ journalctl --vacuum-size=100M
```

Now Nix should be able to garbage-collect!

```shell
λ nix-collect-garbage -d
```

And lastly verify that it WAI'd:

```
λ df -h
Filesystem                Size  Used Avail Use% Mounted on
devtmpfs                  399M     0  399M   0% /dev
tmpfs                     3.9G     0  3.9G   0% /dev/shm
tmpfs                     2.0G  3.7M  2.0G   1% /run
tmpfs                     3.9G  408K  3.9G   1% /run/wrappers
/dev/disk/by-label/nixos  9.9G  5.1G  4.3G  55% /
tmpfs                     4.0M     0  4.0M   0% /sys/fs/cgroup
tmpfs                     797M     0  797M   0% /run/user/0
```

## Closing Thoughts

Why doesn't Nix just reserve enough space to be able to GC itself? Not sure...

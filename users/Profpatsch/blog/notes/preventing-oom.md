tags: linux
date: 2020-01-25
certainty: likely
status: initial
title: Preventing out-of-memory (OOM) errors on Linux

# Preventing out-of-memory (OOM) errors on Linux

I’ve been running out of memory more and more often lately. I don’t use any swap space because I am of the opinion that 16GB of memory should be sufficient for most daily and professional tasks. Which is generally true, however sometimes I have a runaway filling my memory. Emacs is very good at doing this for example, prone to filling your RAM when you open json files with very long lines.

In theory, the kernel OOM killer should come in and save the day, but the Linux OOM killer is notorious for being extremely … conservative. It will try to free every internal structure it can before even thinking about touching any userspace processes. At that point, the desktop usually stopped responding minutes ago.

Luckily the kernel provides memory statistics for the whole system, as well as single process, and the [`earlyoom`](https://github.com/rfjakob/earlyoom) tool uses those to keep memory usage under a certain limit. It will start killing processes, “heaviest” first, until the given upper memory limit is satisfied again.

On NixOS, I set:

```nix
{
  services.earlyoom = {
    enable = true;
    freeMemThreshold = 5; # <%5 free
  };
}
```

and after activation, this simple test shows whether the daemon is working:

```shell
$ tail /dev/zero
fish: “tail /dev/zero” terminated by signal SIGTERM (Polite quit request)
```

`tail /dev/zero` searches for the last line of the file `/dev/zero`, and since it cannot know that there is no next line and no end to the stream of `\0` this file produces, it will fill the RAM as quickly as physically possible. Before it can fill it completely, `earlyoom` recognizes that the limit was breached, singles out the `tail` command as the process using the most amount of memory, and sends it a `SIGTERM`.

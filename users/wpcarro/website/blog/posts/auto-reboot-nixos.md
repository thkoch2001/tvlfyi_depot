## Show me the codes

Regularly rebooting machines can be a useful, hygienic practice, but quite
frankly I cannot be relied on to remember to regularly reboot my machine.

Let's free-up some wetware-RAM by automating this with Nix. The following
addition to your `configuration.nix` will schedule daily reboots at `03:00`:

```nix
systemd.timers.auto-reboot = {
  wantedBy = [ "timers.target" ];
  timerConfig = {
    OnCalendar = "*-*-* 03:00:00";
    Unit = "reboot.target";
  };
};
```

If you want to fiddle with the date format, `systemd-analyze` is your friend:

```shell
λ systemd-analyze calendar '*-*-* 03:00:00'
Normalized form: *-*-* 03:00:00
    Next elapse: Tue 2022-02-01 03:00:00 PST
       (in UTC): Tue 2022-02-01 11:00:00 UTC
       From now: 12h left
```

After calling `nixos-rebuild switch`, you can verify that `systemd` started the
timer with:

```shell
λ systemctl list-timers auto-reboot
#  output omitted because I'm writing this from a different machine
```

## That's all, folks!

I wanted to keep this post short-and-sweet, to build the habit of posting more
regularly. Hopefully someone out there found this useful.

# IRC, GCP, and NixOS

- "cannot read /var/lib/acme/wpcarro.dev/full.pem"
- `sudo stat /var/lib/acme/wpcarro.dev/full.pem` exists
- `sudo -i`
- `su quassel` # denied
- `sudo --user=quassel stat /var/lib/acme/wpcarro.dev/full.pem` exists
- `groups quassel` quassel
- `usermod -a -G nginx quassel` exists
- `groups quassel` quassel, nginx
- `sudo --user=quassel cat /var/lib/acme/wpcarro.dev/full.pem` exists

# Firewall

- `nmap localhost`
- `nmap wpcarro.dev`
- Update `configuration.nix` firewall
- `nmap localhost`
- `nmap wpcarro.dev`
- Edit cloud.google.com Configuration (VPC > Firewall > 6697)

# Quassel

- Test connecting, disconnecting, persisted logs?
- Change `~quassel@253.253.209.35.bc.googleusercontent.com` -> `~quassel@wpcarro.dev`
  - cloaking?
  - rDNS?
    - `dig wpcarro.dev`       -> `35.209.253.253`
    - `dig -x 35.209.253.253` -> `253.253.209.35.bc.googleusercontent.com`
    - From within GCP https://stackoverflow.com/a/47060002 (create the PTR record)
- `/msg hostserv take hackint/user/$account` add cloaking
- disconnect/connect from hackint for changes to take affect
- `/msg hostserv drop` remove cloaking
- Test can I log-in from another machine?

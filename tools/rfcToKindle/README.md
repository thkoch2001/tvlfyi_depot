# rfcToKindle

Wirelessly transfer RFC documents to your Kindle to device for an alternative
medium for reading.

## Installation

`rfcToKindle` makes use of [`buildGo.nix`][2] to package itself.  If you're
using [Nix][1], you can install `rfcToKindle` using `nix-env`:

```shell
> nix-env -f https://github.com/wpcarro/rfcToKindle -i
```

## Usage

```shell
> rfcToKindle -document rfc6479 -recipient username@kindle.com
```

## Dependencies

This uses `sendgmr` to send the file to the Kindle. Make sure:
1. That `sendgmr` is installed and available on $PATH.
2. That it is configured to work with your preferred email address.
3. That the email address `sendgmr` is configured to use is whitelisted in
   your Kindle "Personal Document Settings".

[1]: https://nixos.org/nix/
[2]: https://git.tazj.in/tree/nix/buildGo

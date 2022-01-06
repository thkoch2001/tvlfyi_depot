## Prelude

This is a short story about how I configured myself out of my own email. Posting
this as an exercise in humility, a tutorial for my future self in case of
amnesia, and penance for my sins.

## Background

-   I have 2x Gmail accounts: **work** and **personal**.
-   I configure **work** to send emails as **personal**.
-   I configure **personal** to forward incoming emails to **work**.

This allows me to use **work** and manage both of my inboxes as one. I recently
added two-factor authentication (2FA) to **personal**, forgot about it, and
spent a few days unable to send **personal** emails from any **work** device.

## Symptoms

Whenever I tried to send emails on behalf of **personal**, I'd receive the
following error message as a reply:

> You're sending this from a different address using the 'Send mail as' feature.
> The settings for your 'Send mail as' account are misconfigured or out of date.
> Check those settings and try resending.

Useful error message if you ask me (especially in retrospect), but because I had
*forgotten* that I setup 2FA for **personal**, I naively assumed this issue
might magically disappear given enough time... kind of how restarting your
device resets the state and causes the symptoms of a certain class of bugs to
disappear.

After a few days of mounting frustration, I decided to take a closer look...

## Solution

-   Create an "App Password" for **personal**:
    [instructions](https://support.google.com/accounts/answer/185833?hl=en).
-   Login to **work** and delete **personal** from `Settings > Accounts > Send
    mail as`.
-   `Add another email address` for **personal** using the "App Password" you
    just created.

And now I'm back in business!

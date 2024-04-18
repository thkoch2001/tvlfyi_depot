# My SSH public keys
{ ... }:

let withAll = keys: keys // { all = builtins.attrValues keys; };
in withAll {
  tverskoy = "sk-ecdsa-sha2-nistp256@openssh.com AAAAInNrLWVjZHNhLXNoYTItbmlzdHAyNTZAb3BlbnNzaC5jb20AAAAIbmlzdHAyNTYAAABBBAWvA3RpXpMAqruUbB+eVgvvHCzhs5R9khFRza3YSLeFiIqOxVVgyhzW/BnCSD9t/5JrqRdJIGQLnkQU9m4REhUAAAAEc3NoOg== tazjin@tverskoy";
  tverskoy_ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1fGWz/gsq+ZeZXjvUrV+pBlanw1c3zJ9kLTax9FWQy tazjin@tverskoy";
  zamalek_sk = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIOAw3OaPAjnC6hArGYEmBoXhPf7aZdRGlDZcSqm6gbB8AAAABHNzaDo= tazjin@zamalek";
  zamalek_ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDBRXeb8EuecLHP0bW4zuebXp4KRnXgJTZfeVWXQ1n1R tazjin@zamalek";
  khamovnik_yk = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBPgOyR4rRM8IaVGgN2ZxGlKtd7GLYbxdRTRa3u9EhRNSkHAvRTN9sgw7mm0iPLnHChPy10anKV43vTaIm906Gm8=";
  khamovnik_agenix = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG4YSl5+DHQR3rOoBJLQfQ840U0CrYkByMKdzu/LDxoT tazjin@khamovnik";
}

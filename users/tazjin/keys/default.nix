# My SSH public keys
{ ... }:

let withAll = keys: keys // { all = builtins.attrValues keys; };
in withAll {
  tverskoy = "sk-ecdsa-sha2-nistp256@openssh.com AAAAInNrLWVjZHNhLXNoYTItbmlzdHAyNTZAb3BlbnNzaC5jb20AAAAIbmlzdHAyNTYAAABBBAWvA3RpXpMAqruUbB+eVgvvHCzhs5R9khFRza3YSLeFiIqOxVVgyhzW/BnCSD9t/5JrqRdJIGQLnkQU9m4REhUAAAAEc3NoOg== tazjin@tverskoy";
  tverskoy_ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1fGWz/gsq+ZeZXjvUrV+pBlanw1c3zJ9kLTax9FWQy tazjin@tverskoy";
  zamalek_sk = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIOAw3OaPAjnC6hArGYEmBoXhPf7aZdRGlDZcSqm6gbB8AAAABHNzaDo= tazjin@zamalek";
  zamalek_ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDBRXeb8EuecLHP0bW4zuebXp4KRnXgJTZfeVWXQ1n1R tazjin@zamalek";
  khamovnik_yk = "ecdsa-sha2-nistp256-cert-v01@openssh.com AAAAKGVjZHNhLXNoYTItbmlzdHAyNTYtY2VydC12MDFAb3BlbnNzaC5jb20AAAAgJeYtpc2+Dk3KK5jrecf3o64kQNQz5JzhVaMzdjumRQEAAAAIbmlzdHAyNTYAAABBBCHuKNeGOjar54T6rg7YO4yfDysrwOdPRsixDbJ0yuVioywPMZ79ZTzvT7KfgzOGpggk7dK5FxqEw9uqGVNpmrMXbMbGo9Sv4wAAAAEAAABDc2tvdHR5OnNlY3VyZTp0YXpqaW46eTEyNTc5Nzg0OmY2NjgwMGRmLTcxOGItNDNhOC1hMDBkLWFmMGY5NzYyNDE3NgAAAAoAAAAGdGF6amluAAAAAGSb9XQAAAAAZRKcdAAAAAAAAACCAAAAFXBlcm1pdC1YMTEtZm9yd2FyZGluZwAAAAAAAAAXcGVybWl0LWFnZW50LWZvcndhcmRpbmcAAAAAAAAAFnBlcm1pdC1wb3J0LWZvcndhcmRpbmcAAAAAAAAACnBlcm1pdC1wdHkAAAAAAAAADnBlcm1pdC11c2VyLXJjAAAAAAAAAAAAAABoAAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJnF1UG50PejZLaFFAWTMOL7e4xy44Z/mDJyF6RTsQsIxFN2oC9E4cwOTKSf2Ko/jemdnDOWu+j8X6f4y2KTV5MAAABlAAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAABKAAAAIQDUThJNcCEGINS1x/2fY9BJ6p9/MfrzzM9sccS4Pg5xjwAAACEAnbESV5vl7SUNXA6DsR7qpzO3ZdaeQynjFYUnAJcQzcI=";
}

# My SSH public keys
{ ... }:

let withAll = keys: keys // { all = builtins.attrValues keys; };
in withAll {
  tverskoy = "sk-ecdsa-sha2-nistp256@openssh.com AAAAInNrLWVjZHNhLXNoYTItbmlzdHAyNTZAb3BlbnNzaC5jb20AAAAIbmlzdHAyNTYAAABBBAWvA3RpXpMAqruUbB+eVgvvHCzhs5R9khFRza3YSLeFiIqOxVVgyhzW/BnCSD9t/5JrqRdJIGQLnkQU9m4REhUAAAAEc3NoOg== tazjin@tverskoy";
  tverskoy_ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1fGWz/gsq+ZeZXjvUrV+pBlanw1c3zJ9kLTax9FWQy tazjin@tverskoy";
  zamalek_sk = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIOAw3OaPAjnC6hArGYEmBoXhPf7aZdRGlDZcSqm6gbB8AAAABHNzaDo= tazjin@zamalek";
  zamalek_ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDBRXeb8EuecLHP0bW4zuebXp4KRnXgJTZfeVWXQ1n1R tazjin@zamalek";
  khamovnik_yk = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC7G+RWknTydjXe971ZkwUCAvKCSe1H+j9Zzz/nAHGsKU5Rs9VNnbB0Vzq5HzXcvvy68B8XG8Iecrgn3AeUmiKHIgg0MCI+B7cSc+OXZHfh+J+9YFZiTaVhO5NDfqFgkmtd9jxBV/1FvcweBLFocm0z+gZ7dNDuEECvmlai9stkrCuRMUEMlow5GaJtbdRGUljMse294XqN7kUV1DHOA1vNOp2qr2sDCxd9r2L2optCmIB1PxOM/XNizRn2w9yA5nDR673UCHvr2pDXfItYswfdo10Zbe+BcNkjT8TiHAsVBCBaKytUdqXk53H8SHMPHHhQH+HRrnZMOfWbxag7n+e5 tazjin@khamovnik";
}

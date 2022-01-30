# SSH public keys
{ ... }:

rec {
  sk-ecljg09 = "sk-ecdsa-sha2-nistp256@openssh.com AAAAInNrLWVjZHNhLXNoYTItbmlzdHAyNTZAb3BlbnNzaC5jb20AAAAIbmlzdHAyNTYAAABBBBwJ7dJJUkvIK+bDsVsCsCZSlbs90aOLsHN7XesC8/AmLA5rIRLO8I5ADoOjsWAXl/WAgxqOMmB4LxZjoXWa1a0AAAAEc3NoOg== riking@sk-ECLJG09";
  sk-portable1 = "sk-ecdsa-sha2-nistp256@openssh.com AAAAInNrLWVjZHNhLXNoYTItbmlzdHAyNTZAb3BlbnNzaC5jb20AAAAIbmlzdHAyNTYAAABBBCfA8/0nKk4jXclWHjRZIuicPeyIo9oDwahpnWjEATr7YaFDAo632KTSgqlW0lpx8lX9alLsJRhFV2XaSurYw/EAAAAEc3NoOg== riking@sk-portable1";
  sk-portable2 = "sk-ecdsa-sha2-nistp256@openssh.com AAAAInNrLWVjZHNhLXNoYTItbmlzdHAyNTZAb3BlbnNzaC5jb20AAAAIbmlzdHAyNTYAAABBBEX3DXreQR93SR68QZHTdaVd5RjlRM8C0jcZ4kI4OZwqk7xuk68w3g22q2OM7O+chj+n1N3u0hLxi82QfRnwyasAAAAEc3NoOg== riking@sk-portable2";
  sk-desktop = "sk-ecdsa-sha2-nistp256@openssh.com AAAAInNrLWVjZHNhLXNoYTItbmlzdHAyNTZAb3BlbnNzaC5jb20AAAAIbmlzdHAyNTYAAABBBB+JvN8nAxD+yo49Ohf/UDq7Z049yvkURJIA1XNbvKaAkvfWnCN5m9vTC1FyGxTyCwy4QpD1pFP5fIn0X/kvvfgAAAAEc3NoOg== riking@sk-kane-DAN-A4";

  u2f = [ sk-ecljg09 sk-portable1 sk-portable2 sk-desktop ];

  ed1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAjWIfFH2bAWMZG+HudV1MVHWUl83M/ZgEu6S3SLatYN riking@kane-DAN-A4";
  ed2 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICBblB4C9IgAijv+qN6Zs8TM2Sz7phQvVmRrcDn4VYNo riking@ECLJG09";

  passworded = [ ed1 ed2 ];

  unprotected = [ ];

  all = u2f ++ passworded ++ unprotected;
}

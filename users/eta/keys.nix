{ ... }:

let
  keys = {
    yubikey4 = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQD6E1wuWaXQARNoLnmlOJndwI7/ms3Ga7MJxsUvFtaSiy3g8h/hz4WgyR7YT+hUYjFihh/YkGS9Zy9aEqAa5zBGLcZtgj1O0qOl2joynm679zdlcwAart74fXSJYYupT9tFeXXeWLO1g054lVJ5xZ9KLpBBk+6yzlmmm5KuoitKBqBbadzsqAeKhNn1Nq9ITPU4vxTFk+sXp/nxk/JoUOM8S2N4YuoX9OVenDHKh9DtOcvDZhlosGmunO33/YaU2XB95ZE6cNhEtVlkbyR3a2SsAYz1qGgfH0HSyoK3LJoAM4Aiz99ktuKiI/zMy4k4TV00OCi1sCPEjzUoijZRZt5FMH/TVr9dJROVjHcL9g9//fW3jwqojf7uuJFlTJb47RxjTk4Jb4F6K7HhOs7bgh3WuOjvhyRYbCYcg+RfnwjJk+hfM5GcjZ8J4UZdNc5LyIcfH8W1v9DADBCgz7QcmfrfMloYtEgjK/5XVrtBtiMtUOgpfKujawF55d1Vj26+CxeID8NHMXzZYEMeyRpi/WXlC+lq1Wx4Fj8gvideOw/3gAdj2G3SJWdSPk8XpIFQ1fm3tXB0ltyV5TszIJhfMnmsKJeEm3YlTCR1sMW7nr3wEdMqa6mpcWZTWU+dppmAGr2c+OGSnXkCi7Z2h/YJE6X+izrOrqRspG2fCM8GlfRFWw== cardno:000607469311";
    yubikey5 = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBKCJx23px0Vknw1NlD+arcqeVXxcogPUMJgF/PGp6wA/tg7hHUKs2udC+gDMYlxQ9IpnWOwZ/9yvqzTDwUU3R/4= YubiKey #15026444 PIV Slot 9a";
  };
  configs = {
    whitby = [ keys.yubikey4 keys.yubikey5 ];
  };
in
configs

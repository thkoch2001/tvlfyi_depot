builtins.getEnv "NIX_TEST_VAR" + (if builtins.getEnv "NO_SUCH_VAR" == "" then "bar" else "bla")

{
  description = "TVL loves flakes";

  outputs = { self }: {
    legacyPackages.x86_64-linux = import ./. {
      localSystem = "x86_64-linux";
    };

    devShells.x86_64-linux.default =
      self.legacyPackages.x86_64-linux.tools.depot-deps.passthru.devShell;
  };
}

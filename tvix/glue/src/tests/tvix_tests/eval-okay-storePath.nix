let
  path = builtins.unsafeDiscardStringContext "${../empty-file}";
  storePath = builtins.storePath path;
in {
  hasContext = builtins.hasContext storePath;
  context = builtins.getContext storePath;
}

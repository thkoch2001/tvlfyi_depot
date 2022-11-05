{
  steps = [
    {
      label = "{{matrix}}";
      command = "echo \"Hi, {{matrix}}\"";
      matrix = [
        "mom"
        "dad"
        "Jimmy"
        "Ellie"
      ];
    }
  ];
}

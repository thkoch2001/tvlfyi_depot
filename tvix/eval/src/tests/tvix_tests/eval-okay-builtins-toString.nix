let
  toStringableSet = {
    __toString = self: self.content;
    content = "Hello World";
  };

  toStringExamples = [
    (toString 1)
    (toString 4.2)
    (toString null)
    (toString false)
    (toString true)
    (toString "foo")
    (toString /etc)
    (toString toStringableSet)
    (toString { __toString = _: toStringableSet; })
  ];
in

toStringExamples ++ [ (toString toStringExamples) ]

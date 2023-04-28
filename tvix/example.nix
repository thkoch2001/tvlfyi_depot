let
  /**Docstring */
  foo = 
    x: 
    /**
     We can document the second lambda 
     y: x+y;
     although we dont know how x was applied from the latest call 
     Specific documentation strings will override this generic description
     This documentation use case might be very rare
    */ 
    y: 
    x + y;



  /**
    Specific documentation
    for foo 1
    at this point documentation is specific and makes more sense
    if there was a generic parent documentation it is hidden from this specific documentation
  */
  bar = foo 1;
in
bar
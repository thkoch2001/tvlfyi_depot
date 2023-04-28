rec {
  add = 
    x:
      /**should not see this*/ 
    y: 
      x+y;
  /**Adds one*/
  addOne = add 1;
}.addOne
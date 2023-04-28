let 
  name = builtins.typeOf [];
  set = {
    /**A Function that returns the id*/
    ${name} = x: x;
  };
in
  set.list
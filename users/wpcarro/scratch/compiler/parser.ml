(****************************************************************************** 
 * Defines a generic parser class.
 ******************************************************************************)

open Vec

exception ParseError of string

type token = string
type state = { i : int; tokens : token vec }

class parser (tokens : token vec) =
  object (self)
    val tokens = tokens
    val mutable i = 0

    method advance = i <- i + 1
    method prev : token option = Vec.get (i - 1) tokens
    method curr : token option = Vec.get i tokens
    method next : token option = Vec.get (i + 1) tokens

    method consume : token option =
      match self#curr with
      | None -> None
      | Some x as res ->
          self#advance;
          res

    method expect (x : token) =
      match self#curr with
      | Some y when x = y -> self#advance
      | _ -> raise (ParseError (Printf.sprintf "Expected %s" x))

    method matches (x : token) : bool =
      match self#curr with
      | None -> false
      | Some y ->
          if x = y then
            begin
              self#advance;
              true
            end
          else false

    method exhausted : bool = i >= Vec.length tokens
  end

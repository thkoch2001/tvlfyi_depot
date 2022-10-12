(*******************************************************************************
 * Defines a generic parser class.
 ******************************************************************************)

exception ParseError of string

type token = string
type state = { i : int; tokens : token array }

let get (i : int) (xs : 'a array) : 'a option =
  if i >= Array.length xs then None else Some xs.(i)

class parser (tokens : token array) =
  object (self)
    val mutable tokens : token array = tokens
    val mutable i = ref 0
    method print_state = Printf.sprintf "{ i = %d; }" !i
    method advance = i := !i + 1
    method prev : token option = get (!i - 1) tokens
    method curr : token option = get !i tokens
    method next : token option = get (!i + 1) tokens

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

    method exhausted : bool = !i >= Array.length tokens
    method state : state = { i = !i; tokens }
  end

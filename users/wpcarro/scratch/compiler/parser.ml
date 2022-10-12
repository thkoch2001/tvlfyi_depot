(*******************************************************************************
 * CLI REPL for an s-expression Lambda Calculus.
 *
 * Wish List:
 * - TODO Fix broken parsing:
 *   - TODO let-expressions `(let x 10 x)`
 *
 * Example Usage:
 *   $ ocamlopt types.ml inference.ml parser.ml && ./a.out
 *   repl> (-> x (-> y x))
 *   a0 -> a1 -> a0
*******************************************************************************)

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
      | Some x ->
          self#advance;
          Some x

    method matches (x : token) : bool =
      match self#curr with
      | None -> false
      | Some y ->
          if x = y then
            let _ = self#advance in
            true
          else false

    method re_matches (re : string) : bool =
      match self#curr with
      | None -> false
      | Some y ->
          (* if Str.string_match (Str.regexp re) y 0 then *)
          if true then
            let _ = self#advance in
            true
          else false

    method exhausted : bool = !i >= Array.length tokens
    method state : state = { i = !i; tokens }
  end

(*******************************************************************************
 *
 * Lambda Calculus Expression Language:
 *
 *   Helpers:
 *     symbol     -> [-a-z]+
 *     boolean    -> 'true' | 'false'
 *     integer    -> [1-9][0-9]*
 *
 *   Core:
 *     expression -> funcdef
 *     funcdef    -> '(' '->' symbol expr ')'
 *                 | binding
 *     binding    -> '(' 'let' symbol expr expr ')'
 *                 | funccall
 *     funccall   -> '(' funcdef expr ')'
 *                 | variable
 *     variable   -> symbol
 *                 | literal
 *     literal -> boolean | integer
 *
 ******************************************************************************)

let to_array (q : 'a Queue.t) : 'a array =
  let result = Array.make (Queue.length q) "" in
  let i = ref 0 in
  Queue.iter
    (fun x ->
      result.(!i) <- x;
      i := !i + 1)
    q;
  result

type literal = LiteralBool of bool | LiteralInt of int

let tokenize (x : string) : token array =
  let q = Queue.create () in
  let i = ref 0 in
  while !i < String.length x do
    match x.[!i] with
    | ' ' -> i := !i + 1
    | '(' ->
        Queue.push "(" q;
        i := !i + 1
    | ')' ->
        Queue.push ")" q;
        i := !i + 1
    | _ ->
        let token = ref "" in
        while !i < String.length x && not (String.contains "() " x.[!i]) do
          token := !token ^ String.make 1 x.[!i];
          i := !i + 1
        done;
        Queue.push !token q
  done;
  to_array q

let parse_symbol (p : parser) : string option =
  if p#re_matches "[-a-z]+" then p#prev else None

let parse_literal (p : parser) : Types.value option =
  match p#curr with
  | Some "true" -> Some (ValueLiteral (LiteralBool true))
  | Some "false" -> Some (ValueLiteral (LiteralBool false))
  | Some x -> Some (ValueLiteral (LiteralInt (int_of_string x)))
  | None -> None

let parse_variable (p : parser) : Types.value option =
  match parse_symbol p with
  | Some x -> Some (ValueVariable x)
  | None -> parse_literal p

let parse_funccall (p : parser) : Types.value option =
  if p#matches "(" then parse_variable p else parse_variable p

let rec parse_expression (p : parser) : Types.value option =
  if not p#exhausted then parse_binding p else None

and parse_funcdef (p : parser) : Types.value option =
  if p#matches "(" && p#matches "->" then
    match (parse_symbol p, parse_expression p) with
    | Some symbol, Some expr -> Some (ValueFunction (symbol, expr))
    | _ -> None
  else parse_binding p

and parse_binding (p : parser) : Types.value option =
  if p#matches "(" && p#matches "let" then
    match (parse_symbol p, parse_expression p, parse_expression p) with
    | Some k, Some v, Some body -> Some (ValueBinder (k, v, body))
    | _ -> None
  else parse_funccall p

and parse_funccall (p : parser) : Types.value option =
  match (parse_funcdef p, parse_expression p) with
  | Some f, Some expr -> None
  | _ -> parse_variable p

let print_tokens (xs : string array) =
  xs |> Array.to_list
  |> List.map (Printf.sprintf "\"%s\"")
  |> String.concat ", "
  |> Printf.sprintf "tokens: [ %s ]"
  |> print_string |> print_newline

let parse_language (x : string) : Types.value option =
  let tokens = tokenize x in
  print_tokens tokens;
  parse_expression (new parser tokens)

let rec pretty (ast : Types.value) : string =
  match ast with
  | ValueLiteral (LiteralBool x) -> string_of_bool x
  | ValueLiteral (LiteralInt x) -> string_of_int x
  | ValueVariable x -> x
  | ValueFunction (x, body) -> Printf.sprintf "(-> %s %s)" x (pretty body)
  | ValueApplication (f, x) -> Printf.sprintf "(%s %s)" (pretty f) (pretty x)
  | ValueBinder (k, v, x) ->
      Printf.sprintf "(let %s %s %s)" k (pretty v) (pretty x)

let print_ast (ast : Types.value) : Types.value =
  ast |> pretty |> Printf.sprintf "ast: %s" |> print_string |> print_newline;
  ast
;;

while true do
  let _ = print_string "repl> " in
  let x = read_line () in
  match parse_language x with
  | Some ast ->
      ast |> print_ast |> Inference.do_infer |> Inference.pretty
      |> Printf.sprintf "type: %s" |> print_string |> print_newline
  | None -> "Could not parse" |> print_string |> print_newline
done

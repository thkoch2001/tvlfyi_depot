(*******************************************************************************
 * CLI REPL for an s-expression Lambda Calculus.
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
 *     binding    -> '(' 'let' symbol expr expr ')'
 *     funcdef    -> '(' 'fn' symbol expr ')'
 *     funccall   -> '(' ( symbol | funcdef) expr ')'
 *     literal    -> boolean | integer
 *     variable   -> symbol
 *
 * Example Usage:
 *   $ ocamlopt types.ml str.cmxa inference.ml parser.ml expr_parser.ml && ./a.out
 *   repl> true
 *   tokens: [ "true" ]
 *   ast: ValueLiteral (LiteralBool true)
 *   Boolean
 *   repl>
 *
 ******************************************************************************)

open Parser
open Inference

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

let ( let* ) = Option.bind
let map = Option.map

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
  let* x = p#curr in
  if Str.string_match (Str.regexp "[-a-z][0-9]*") x 0 then
    begin
      p#advance;
      Some x
    end
  else
    None

let parse_variable (p : parser) : Types.value option =
  let* x = parse_symbol p in
  Some (Types.ValueVariable x)

let parse_literal (p : parser) : Types.value option =
  match p#curr with
  | Some "true" ->
     p#advance;
     Some (ValueLiteral (LiteralBool true))
  | Some "false" ->
     p#advance;
     Some (ValueLiteral (LiteralBool false))
  | Some x ->
     (match int_of_string_opt x with
      | Some n ->
         p#advance;
         Some (ValueLiteral (LiteralInt n))
      | _ -> parse_variable p)
  | _ -> None

let rec parse_expression (p : parser) : Types.value option =
  parse_binding p

and parse_funccall (p : parser) : Types.value option =
  match (p#curr, p#next) with
  | (Some "(", Some "(") ->
     p#advance;
     let* f = parse_funcdef p in
     let* x = parse_expression p in
     p#expect ")";
     Some (Types.ValueApplication (f, x))
  | (Some "(", _) ->
     p#advance;
     let* f = parse_symbol p in
     let* x = parse_expression p in
     p#expect ")";
     Some (Types.ValueVarApplication (f, x))
  | _ -> parse_literal p

and parse_funcdef (p : parser) : Types.value option =
  match (p#curr, p#next) with
  | (Some "(", Some "fn") ->
     p#advance;
     p#advance;
     let* name = parse_symbol p in
     let* body = parse_expression p in
     p#expect ")";
     Some (Types.ValueFunction (name, body))
  | _ -> parse_funccall p

and parse_binding (p : parser) : Types.value option =
  match (p#curr, p#next) with
  | (Some "(", Some "let") ->
     p#advance;
     p#advance;
     let* name = parse_symbol p in
     let* value = parse_expression p in
     let* body = parse_expression p in
     Some (Types.ValueBinder (name, value, body))
  | _ -> parse_funcdef p

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

let rec debug (ast : Types.value) : string =
  match ast with
  | ValueLiteral (LiteralBool x) ->
     Printf.sprintf "ValueLiteral (LiteralBool %s)" (string_of_bool x)
  | ValueLiteral (LiteralInt x) ->
     Printf.sprintf "ValueLiteral (LiteralInt %s)" (string_of_int x)
  | ValueVariable x ->
     Printf.sprintf "ValueVariable %s" x
  | ValueFunction (x, body) ->
     Printf.sprintf "ValueFunction (%s, %s)" x (debug body)
  | ValueApplication (f, x) ->
     Printf.sprintf "ValueApplication (%s, %s)" (debug f) (debug x)
  | ValueVarApplication (f, x) ->
     Printf.sprintf "ValueVarApplication (%s, %s)" f (debug x)
  | ValueBinder (k, v, x) ->
      Printf.sprintf "ValueBinder (%s, %s, %s)" k (debug v) (debug x)

let debug_ast (ast : Types.value) : Types.value =
  ast |> debug |> Printf.sprintf "ast: %s" |> print_string |> print_newline;
  ast

let main =
  while true do
    begin
      print_string "repl> ";
      let x = read_line () in
      match parse_language x with
      | Some ast ->
         (match ast |> debug_ast |> do_infer with
          | None ->
             "Type-check failed"
             |> print_string
             |> print_newline
          | Some x ->
             x
             |> Types.pretty
             |> print_string
             |> print_newline)
      | None ->
         "Could not parse"
         |> print_string
         |> print_newline
    end
  done

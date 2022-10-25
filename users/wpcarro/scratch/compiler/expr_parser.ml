(*******************************************************************************
 * CLI REPL for an s-expression Lambda Calculus.
 *
 * Lambda Calculus Expression Language:
 *
 *   Helpers:
 *     symbol     -> [-a-z]+
 *     string     -> '"' [^"]* '"'
 *     boolean    -> 'true' | 'false'
 *     integer    -> [1-9][0-9]*
 *
 *   Core:
 *     expression -> funcdef
 *     binding    -> '(' 'let' symbol expr expr ')'
 *     funcdef    -> '(' 'fn' symbol expr ')'
 *     funccall   -> '(' ( symbol | funcdef) expr ')'
 *     literal    -> string | boolean | integer
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
open Debug
open Prettify
open Vec

type literal = LiteralBool of bool | LiteralInt of int

let ( let* ) = Option.bind
let map = Option.map

let tokenize (x : string) : token vec =
  let xs = Vec.create () in
  let i = ref 0 in
  while !i < String.length x do
    match x.[!i] with
    | ' ' -> i := !i + 1
    (* strings *)
    | '"' ->
        let curr = ref "\"" in
        i := !i + 1;
        while x.[!i] != '"' do
          curr := !curr ^ "?";
          i := !i + 1
        done;
        curr := !curr ^ "\"";
        Vec.append !curr xs;
        i := !i + 1
    | '(' ->
        Vec.append "(" xs;
        i := !i + 1
    | ')' ->
        Vec.append ")" xs;
        i := !i + 1
    | _ ->
        let token = ref "" in
        while !i < String.length x && not (String.contains "() " x.[!i]) do
          token := !token ^ String.make 1 x.[!i];
          i := !i + 1
        done;
        Vec.append !token xs
  done;
  xs

let parse_symbol (p : parser) : string option =
  let* x = p#curr in
  if Str.string_match (Str.regexp "[-a-z][0-9]*") x 0 then (
    p#advance;
    Some x)
  else None

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
  | Some x -> (
      match int_of_string_opt x with
      | Some n ->
          p#advance;
          Some (ValueLiteral (LiteralInt n))
      | _ ->
          if String.starts_with ~prefix:"\"" x then (
            p#advance;
            Some (ValueLiteral (LiteralString x)))
          else parse_variable p)
  | _ -> None

let rec parse_expression (p : parser) : Types.value option = parse_binding p

and parse_funccall (p : parser) : Types.value option =
  match (p#curr, p#next) with
  | Some "(", Some "(" ->
      p#advance;
      let* f = parse_funcdef p in
      let* x = parse_expression p in
      p#expect ")";
      Some (Types.ValueApplication (f, x))
  | Some "(", _ ->
      p#advance;
      let* f = parse_symbol p in
      let* x = parse_expression p in
      p#expect ")";
      Some (Types.ValueVarApplication (f, x))
  | _ -> parse_literal p

and parse_funcdef (p : parser) : Types.value option =
  match (p#curr, p#next) with
  | Some "(", Some "fn" ->
      p#advance;
      p#advance;
      let* name = parse_symbol p in
      let* body = parse_expression p in
      p#expect ")";
      Some (Types.ValueFunction (name, body))
  | _ -> parse_funccall p

and parse_binding (p : parser) : Types.value option =
  match (p#curr, p#next) with
  | Some "(", Some "let" ->
      p#advance;
      p#advance;
      let* name = parse_symbol p in
      let* value = parse_expression p in
      let* body = parse_expression p in
      Some (Types.ValueBinder (name, value, body))
  | _ -> parse_funcdef p

let print_tokens (xs : string vec) : unit =
  xs
  |> Vec.map (Printf.sprintf "\"%s\"")
  |> Vec.join ", "
  |> Printf.sprintf "tokens: [ %s ]"
  |> print_string |> print_newline

let parse_language (x : string) : Types.value option =
  let tokens = tokenize x in
  print_tokens tokens;
  parse_expression (new parser tokens)

let main =
  while true do
    print_string "repl> ";
    let x = read_line () in
    match parse_language x with
    | Some ast -> (
        match ast |> Debug.print Debug.ast "ast" |> do_infer with
        | None -> "Type-check failed" |> print_string |> print_newline
        | Some x -> x |> Prettify.type' |> print_string |> print_newline)
    | None -> "Could not parse" |> print_string |> print_newline
  done

(******************************************************************************
 * Type Expression Language:
 *
 * Helpers:
 *   symbol   -> [a-z]
 *
 * Core:
 *   type     -> function
 *   function -> ( variable | literal ) '->' type
 *   literal  -> 'Integer' | 'Boolean'
 *   variable -> symbol
 ******************************************************************************)

open Types
open Prettify
open Parser
open Inference
open Vec

type side = LHS | RHS

let ( let* ) = Option.bind

let printsub (s : substitution) =
  s |> Debug.substitution |> print_string |> print_newline

let tokenize (x : string) : token vec =
  let xs = Vec.create () in
  let i = ref 0 in
  while !i < String.length x do
    match x.[!i] with
    | ' ' -> i := !i + 1
    | _ ->
       let beg = !i in
       while (!i < String.length x) && (x.[!i] != ' ') do
         i := !i + 1
       done;
       Vec.append (String.sub x beg (!i - beg)) xs
  done;
  xs

let rec parse_type (p : parser) : _type option =
  parse_function p
and parse_function (p : parser) : _type option =
  match p#next with
  | Some "->" ->
     let* a = parse_literal p in
     p#advance;
     let* b = parse_type p in
     Some (TypeArrow (a, b))
  | _ -> parse_literal p
and parse_literal (p : parser) : _type option =
  match p#curr with
  | Some "Integer" | Some "Int" -> p#advance; Some TypeInt
  | Some "Boolean" | Some "Bool" -> p#advance; Some TypeBool
  | Some _ -> parse_variable p
  | None -> None
and parse_variable (p : parser) : _type option =
  match p#curr with
  | Some x when String.length x = 1 -> p#advance; Some (TypeVariable x)
  | _ -> None

let print_tokens (xs : string vec) =
  xs
  |> Vec.map (Printf.sprintf "\"%s\"")
  |> Vec.join ", "
  |> Printf.sprintf "tokens: [ %s ]"
  |> print_string 
  |> print_newline

let print_type (t : _type) =
  t |> Debug.type' |> Printf.sprintf "type: %s" |> print_string |> print_newline

let parse_input (x : string) : _type option =
  let tokens = tokenize x in
  print_tokens tokens;
  parse_type (new parser tokens)

(* Continually prompt until user provides a parseable type expression *)
let rec read_type (arg : side) : _type =
  let prompt = match arg with
    | LHS -> "lhs> "
    | RHS -> "rhs> " in
  print_string prompt;
  let x = read_line () in
  match parse_input x with
  | None ->
     print_string "Failed to parse input.\n";
     read_type arg
  | Some ast ->
     print_type ast;
     ast

let main =
  while true do
    begin
      let lhs = read_type LHS in
      let rhs = read_type RHS in
      match unify lhs rhs with
      | None ->
         Printf.printf "Cannot unify \"%s\" with \"%s\"\n" (Debug.type' lhs) (Debug.type' rhs)
      | Some x -> printsub x
    end
  done

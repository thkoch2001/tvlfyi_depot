type literal = LiteralInt of int | LiteralBool of bool

(* Lambda Calculus definition *)
type value =
  | ValueLiteral of literal
  | ValueVariable of string
  | ValueFunction of string * value
  | ValueApplication of value * value
  | ValueVarApplication of string * value
  | ValueBinder of string * value * value

let rec debug_value (x : value) : string =
  match x with
  | ValueLiteral (LiteralInt x) ->
     Printf.sprintf "Int %d" x
  | ValueLiteral (LiteralBool x) ->
     Printf.sprintf "Bool %b" x
  | ValueVariable x ->
     Printf.sprintf "Var %s" x
  | ValueFunction (name, x) ->
     Printf.sprintf "Fn %s %s" name (debug_value x)
  | ValueApplication (f, x) ->
     Printf.sprintf "App %s %s" (debug_value f) (debug_value x)
  | ValueVarApplication (name, x) ->
     Printf.sprintf "App %s %s" name (debug_value x)
  | ValueBinder (name, x, body) ->
     Printf.sprintf "Bind %s %s %s" name (debug_value x) (debug_value body)

module FromString = Map.Make (String)

type _type =
  | TypeInt
  | TypeBool
  | TypeVariable of string
  | TypeArrow of _type * _type

let rec debug_type (t : _type) : string =
  match t with
  | TypeInt -> "Integer"
  | TypeBool -> "Boolean"
  | TypeVariable k -> Printf.sprintf "%s" k
  | TypeArrow (a, b) -> Printf.sprintf "%s -> %s" (debug_type a) (debug_type b)

type quantified_type = QuantifiedType of string list * _type

let debug_quantified_type (q : quantified_type) : string =
  let QuantifiedType (vars, t) = q in
  if List.length vars == 0 then
    Printf.sprintf "%s" (debug_type t)
  else
    Printf.sprintf "forall %s. %s" (String.concat "," vars) (debug_type t)

type set = bool FromString.t
type substitution = _type FromString.t

let debug_substitution (s : substitution) : string =
  FromString.fold (fun k v acc -> Printf.sprintf "%s\"%s\" |-> %s;" acc k (debug_type v)) s ""
  |> Printf.sprintf "{ %s }"

type env = quantified_type FromString.t

let debug_env (s : env) : string =
  FromString.fold (fun k v acc -> Printf.sprintf "%s\"%s\" |-> %s;" acc k (debug_quantified_type v)) s ""
  |> Printf.sprintf "{ %s }"

type inference = Inference of substitution * _type

let debug_inference (Inference (s, t)) =
  Printf.sprintf "type: %s; sub: %s" (debug_type t) (debug_substitution s)

let rec pretty (t : _type) : string =
  match t with
  | TypeInt -> "Integer"
  | TypeBool -> "Boolean"
  | TypeVariable k -> Printf.sprintf "%s" k
  | TypeArrow (a, b) -> Printf.sprintf "%s -> %s" (pretty a) (pretty b)

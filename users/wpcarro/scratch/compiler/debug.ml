open Types

(* Print x prefixed with tag and return x unchanged. *)
let print (f : 'a -> string) (tag : string) (x : 'a) : 'a =
  Printf.printf "%s: %s\n" tag (f x);
  x

let rec ast (tree : Types.value) : string =
  match tree with
  | ValueLiteral (LiteralBool x) ->
      Printf.sprintf "ValueLiteral (LiteralBool %s)" (string_of_bool x)
  | ValueLiteral (LiteralInt x) ->
      Printf.sprintf "ValueLiteral (LiteralInt %s)" (string_of_int x)
  | ValueLiteral (LiteralString x) ->
      Printf.sprintf "ValueLiteral (LiteralString %s)" x
  | ValueVariable x -> Printf.sprintf "ValueVariable %s" x
  | ValueFunction (x, body) ->
      Printf.sprintf "ValueFunction (%s, %s)" x (ast body)
  | ValueApplication (f, x) ->
      Printf.sprintf "ValueApplication (%s, %s)" (ast f) (ast x)
  | ValueVarApplication (f, x) ->
      Printf.sprintf "ValueVarApplication (%s, %s)" f (ast x)
  | ValueBinder (k, v, x) ->
      Printf.sprintf "ValueBinder (%s, %s, %s)" k (ast v) (ast x)

let rec value (x : value) : string =
  match x with
  | ValueLiteral (LiteralInt x) -> Printf.sprintf "Int %d" x
  | ValueLiteral (LiteralBool x) -> Printf.sprintf "Bool %b" x
  | ValueLiteral (LiteralString x) -> Printf.sprintf "String %s" x
  | ValueVariable x -> Printf.sprintf "Var %s" x
  | ValueFunction (name, x) -> Printf.sprintf "Fn %s %s" name (value x)
  | ValueApplication (f, x) -> Printf.sprintf "App %s %s" (value f) (value x)
  | ValueVarApplication (name, x) -> Printf.sprintf "App %s %s" name (value x)
  | ValueBinder (name, x, body) ->
      Printf.sprintf "Bind %s %s %s" name (value x) (value body)

let rec type' (t : _type) : string =
  match t with
  | TypeInt -> "Integer"
  | TypeBool -> "Boolean"
  | TypeString -> "String"
  | TypeVariable k -> Printf.sprintf "%s" k
  | TypeArrow (a, b) -> Printf.sprintf "%s -> %s" (type' a) (type' b)

let quantified_type (q : quantified_type) : string =
  let (QuantifiedType (vars, t)) = q in
  if List.length vars == 0 then Printf.sprintf "%s" (type' t)
  else Printf.sprintf "forall %s. %s" (String.concat "," vars) (type' t)

let substitution (s : substitution) : string =
  FromString.fold
    (fun k v acc -> Printf.sprintf "%s\"%s\" |-> %s;" acc k (type' v))
    s ""
  |> Printf.sprintf "{ %s }"

let env (s : env) : string =
  FromString.fold
    (fun k v acc -> Printf.sprintf "%s\"%s\" |-> %s;" acc k (quantified_type v))
    s ""
  |> Printf.sprintf "{ %s }"

let inference (Inference (s, t)) =
  Printf.sprintf "type: %s; sub: %s" (type' t) (substitution s)

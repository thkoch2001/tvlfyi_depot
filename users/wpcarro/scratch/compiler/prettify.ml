open Types

(* Pretty-print the type, t. *)
let rec type' (t : _type) : string =
  match t with
  | TypeInt -> "Integer"
  | TypeBool -> "Boolean"
  | TypeVariable k -> Printf.sprintf "%s" k
  | TypeArrow (a, b) -> Printf.sprintf "%s -> %s" (type' a) (type' b)

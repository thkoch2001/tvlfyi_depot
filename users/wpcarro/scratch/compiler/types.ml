type literal = LiteralInt of int | LiteralBool of bool

(* Lambda Calculus definition *)
type value =
  | ValueLiteral of literal
  | ValueVariable of string
  | ValueFunction of string * value
  | ValueApplication of value * value
  | ValueVarApplication of string * value
  | ValueBinder of string * value * value

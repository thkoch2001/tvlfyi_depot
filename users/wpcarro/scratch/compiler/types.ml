type literal =
  | LiteralInt of int
  | LiteralBool of bool
  | LiteralString of string

(* Lambda Calculus definition *)
type value =
  | ValueLiteral of literal
  | ValueVariable of string
  | ValueFunction of string * value
  | ValueApplication of value * value
  | ValueVarApplication of string * value
  | ValueBinder of string * value * value

module FromString = Map.Make (String)

type _type =
  | TypeInt
  | TypeBool
  | TypeString
  | TypeVariable of string
  | TypeArrow of _type * _type

type quantified_type = QuantifiedType of string list * _type
type set = bool FromString.t
type substitution = _type FromString.t
type env = quantified_type FromString.t
type inference = Inference of substitution * _type

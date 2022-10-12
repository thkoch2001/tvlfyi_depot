(*
  WIP implementation of the Hindley-Milner type system primarily for learning
  purposes.

  Example Usage:
    $ utop -init algorithm_w.ml
    utop # source (ValueApplication (const, ValueLiteral (LiteralInt 10)));;
    - : string = "(x: y: x) (10)"
*)

(*******************************************************************************
 * Types
 ******************************************************************************)

module FromString = Map.Make(String)

type _type
  = TypeVariable of string
  | TypeInt
  | TypeBool
  | TypeArrow of _type * _type

type literal
  = LiteralInt of int
  | LiteralBool of bool

(* Lambda Calculus definition *)
type value
  = ValueLiteral of literal
  | ValueVariable of string
  | ValueFunction of string * value
  | ValueApplication of value * value
  | ValueBinder of string * value * value

(* TODO(wpcarro): Would quantification be a better name? *)
type scheme = Scheme of (string list) * _type

type substitution = _type FromString.t

(* TODO(wpcarro): Would type environment be a better name? *)
type context = scheme FromString.t

type inference = Inference of substitution * _type

(*******************************************************************************
 * Main
 ******************************************************************************)

(* a -> a *)
let id : value =
  ValueFunction ("x", ValueVariable "x")
;;

(* a -> b -> a *)
let const : value =
  ValueFunction ("x", ValueFunction ("y", ValueVariable "x"))
;;

exception Todo of string
let todo msg = raise (Todo msg)

let emptyCtx : context = FromString.empty

let unify (a : _type) (b : _type) : substitution =
  todo "unify"

let composeSubstitutions (ss : substitution list) : substitution =
  todo "composeSubstitutions"

let substituteCtx (s : substitution) (ctx : context) : context =
  todo "substituteCtx"


let substituteType (s : substitution) (t : _type) : _type =
  todo "substituteType"

let i : int ref = ref 0
let makeTypeVar () : _type =
  let res = Printf.sprintf "a%d" !i in
  i := !i + 1;
  TypeVariable res

let instantiate (q : scheme) : _type =
  todo "instantiate"

let generalize (ctx : context) (t : _type) : scheme =
  todo "generalize"

(* Map union that the rhs values (i.e. "last writer wins"). *)
let lww (xs : 'a FromString.t) (ys: 'a FromString.t) : 'a FromString.t =
  FromString.union (fun k x y -> Some y) xs ys

let rec infer (ctx : context) (x : value) : inference =
  match x with
  | ValueLiteral lit ->
     (match lit with
      | LiteralInt _ -> Inference (FromString.empty, TypeInt)
      | LiteralBool _ -> Inference (FromString.empty, TypeBool))
  | ValueVariable k ->
     let v = FromString.find k ctx in
     Inference (FromString.empty, instantiate v)
  | ValueFunction (param, body) ->
     let typevar = makeTypeVar () in
     let ctx1 = FromString.remove param ctx in
     let ctx2 = lww ctx1 (FromString.singleton param (Scheme ([], typevar))) in
     let Inference (s1, t1) = infer ctx2 body in
     Inference (s1, TypeArrow ((substituteType s1 typevar), t1))
  | ValueApplication (f, x) ->
     let typevar = makeTypeVar () in
     let Inference (s1, t1) = infer ctx f in
     let Inference (s2, t2) = infer (substituteCtx s1 ctx) x in
     let s3 = unify (substituteType s2 t1) (TypeArrow (t2, typevar)) in
     Inference (composeSubstitutions [s3; s2; s1], substituteType s3 typevar)
  | ValueBinder (k, v, body) ->
     let Inference (s1, t1) = infer ctx v in
     let ctx1 = FromString.remove k ctx in
     let tg = generalize (substituteCtx s1 ctx) t1 in
     let ctx2 = FromString.add k tg ctx1 in
     let Inference (s2, t2) = infer (substituteCtx s1 ctx2) body in
     Inference (composeSubstitutions [s1; s2], t2)

let rec source (x : value) : string =
  match x with
  | ValueLiteral lit ->
     (match lit with
      | LiteralInt x -> string_of_int x
      | LiteralBool x -> string_of_bool x)
  | ValueVariable x -> x
  | ValueFunction (var, body) ->
     Printf.sprintf "%s: %s" var (source body)
  | ValueApplication (x, y) ->
     Printf.sprintf "(%s) (%s)" (source x) (source y)
  | ValueBinder (x, y, z) ->
     Printf.sprintf "let %s = %s in %s" x (source y) (source z)

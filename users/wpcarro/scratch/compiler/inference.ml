(*
  WIP implementation of the Hindley-Milner type system primarily for learning
  purposes.

  Example Usage:
    $ utop -init algorithm_w.ml
    utop # source (ValueApplication (const, ValueLiteral (LiteralInt 10)));;
    - : string = "(x: y: x) (10)"
    utop # ValueLiteral (LiteralInt 10) |> infer FromString.empty |> unwrap |> pretty;;
    - : string = "Integer"
*)

(*******************************************************************************
 * Types
 ******************************************************************************)

module FromString = Map.Make (String)

type _type =
  | TypeInt
  | TypeBool
  | TypeVariable of string
  | TypeArrow of _type * _type

(* TODO(wpcarro): Would quantification be a better name? *)
type scheme = Scheme of string list * _type
type set = bool FromString.t
type substitution = _type FromString.t

(* TODO(wpcarro): Would type environment be a better name? *)
type context = scheme FromString.t
type inference = Inference of substitution * _type

(*******************************************************************************
 * Expressions
 ******************************************************************************)

(* (:: id (-> a a)) *)
let id : Types.value = ValueFunction ("x", ValueVariable "x")

(* (:: const (-> a (-> b a))) *)
let const : Types.value =
  ValueFunction ("x", ValueFunction ("y", ValueVariable "x"))

(*******************************************************************************
 * Helper Functions
 ******************************************************************************)

let set_from_list (xs : string list) : set =
  xs |> List.fold_left (fun acc x -> FromString.add x true acc) FromString.empty

(* Map union that the rhs values (i.e. "last writer wins"). *)
let lww (xs : 'a FromString.t) (ys : 'a FromString.t) : 'a FromString.t =
  FromString.union (fun k x y -> Some y) xs ys

(*******************************************************************************
 * Main
 ******************************************************************************)

exception Todo of string

let todo msg = raise (Todo msg)
let emptyCtx : context = FromString.empty

let rec freetypevars (t : _type) : set =
  match t with
  | TypeVariable k -> FromString.singleton k true
  | TypeInt -> FromString.empty
  | TypeBool -> FromString.empty
  | TypeArrow (a, b) -> lww (freetypevars a) (freetypevars b)

let i : int ref = ref 0

let makeTypeVar () : _type =
  let res = Printf.sprintf "a%d" !i in
  i := !i + 1;
  TypeVariable res

exception OccursCheck

let bindvar (k : string) (t : _type) : substitution =
  if t == TypeVariable k then FromString.empty
  else if FromString.exists (fun name _ -> name == k) (freetypevars t) then
    raise OccursCheck
  else FromString.singleton k t

let rec instantiate (q : scheme) : _type =
  let (Scheme (names, t)) = q in
  match t with
  | TypeInt -> TypeInt
  | TypeBool -> TypeBool
  | TypeVariable k ->
      if List.exists (( == ) k) names then makeTypeVar () else TypeVariable k
  | TypeArrow (a, b) ->
      TypeArrow
        (instantiate (Scheme (names, a)), instantiate (Scheme (names, b)))

let schemeFTVs (q : scheme) : set =
  let (Scheme (names, t)) = q in
  lww (freetypevars t) (names |> set_from_list)

let generalize (ctx : context) (t : _type) : scheme =
  let ctxftv =
    ctx |> FromString.bindings
    |> List.map (fun (_, v) -> schemeFTVs v)
    |> List.fold_left lww FromString.empty
  in
  let names =
    lww (freetypevars t) ctxftv
    |> FromString.bindings
    |> List.map (fun (k, _) -> k)
  in
  Scheme (names, t)

let rec substituteType (s : substitution) (t : _type) : _type =
  match t with
  | TypeInt -> TypeInt
  | TypeBool -> TypeBool
  | TypeVariable k -> (
      match FromString.find_opt k s with Some v -> v | None -> TypeVariable k)
  | TypeArrow (a, b) -> TypeArrow (substituteType s a, substituteType s b)

let substituteScheme (s : substitution) (q : scheme) : scheme =
  let (Scheme (names, t)) = q in
  let s1 =
    FromString.filter (fun k v -> List.exists (fun x -> k != x) names) s
  in
  Scheme (names, substituteType s1 t)

let substituteCtx (s : substitution) (ctx : context) : context =
  FromString.map (fun q -> substituteScheme s q) ctx

let composeSubstitutions (s1 : substitution) (s2 : substitution) : substitution
    =
  lww (FromString.map (substituteType s1) s2) s1

exception CannotUnify

let rec unify (a : _type) (b : _type) : substitution =
  match (a, b) with
  | TypeInt, TypeInt -> FromString.empty
  | TypeBool, TypeBool -> FromString.empty
  | TypeVariable k, _ -> bindvar k b
  | _, TypeVariable k -> bindvar k b
  | TypeArrow (a, b), TypeArrow (c, d) ->
      let s1 = unify a c in
      let s2 = unify b d in
      composeSubstitutions s1 s2
  | _ -> raise CannotUnify

let rec infer (ctx : context) (x : Types.value) : inference =
  match x with
  | ValueLiteral lit -> (
      match lit with
      | LiteralInt _ -> Inference (FromString.empty, TypeInt)
      | LiteralBool _ -> Inference (FromString.empty, TypeBool))
  | ValueVariable k ->
      let v = FromString.find k ctx in
      Inference (FromString.empty, instantiate v)
  | ValueFunction (param, body) ->
      let typevar = makeTypeVar () in
      let ctx1 = FromString.remove param ctx in
      let ctx2 = lww ctx1 (FromString.singleton param (Scheme ([], typevar))) in
      let (Inference (s1, t1)) = infer ctx2 body in
      Inference (s1, TypeArrow (substituteType s1 typevar, t1))
  | ValueApplication (f, x) ->
      let typevar = makeTypeVar () in
      let (Inference (s1, t1)) = infer ctx f in
      let (Inference (s2, t2)) = infer (substituteCtx s1 ctx) x in
      let s3 = unify (substituteType s2 t1) (TypeArrow (t2, typevar)) in
      Inference
        ( composeSubstitutions (composeSubstitutions s3 s2) s1,
          substituteType s3 typevar )
  | ValueBinder (k, v, body) ->
      let (Inference (s1, t1)) = infer ctx v in
      let ctx1 = FromString.remove k ctx in
      let tg = generalize (substituteCtx s1 ctx) t1 in
      let ctx2 = FromString.add k tg ctx1 in
      let (Inference (s2, t2)) = infer (substituteCtx s1 ctx2) body in
      Inference (composeSubstitutions s1 s2, t2)

let do_infer (x : Types.value) : _type =
  let (Inference (_, t)) = infer FromString.empty x in
  t

let unwrap (x : inference) : _type =
  let (Inference (_, t)) = x in
  t

let rec pretty (t : _type) : string =
  match t with
  | TypeInt -> "Integer"
  | TypeBool -> "Boolean"
  | TypeVariable k -> k
  | TypeArrow (a, b) -> Printf.sprintf "%s -> %s" (pretty a) (pretty b)

let rec source (x : Types.value) : string =
  match x with
  | ValueLiteral lit -> (
      match lit with
      | LiteralInt x -> string_of_int x
      | LiteralBool x -> string_of_bool x)
  | ValueVariable x -> x
  | ValueFunction (var, body) -> Printf.sprintf "%s: %s" var (source body)
  | ValueApplication (x, y) -> Printf.sprintf "(%s) (%s)" (source x) (source y)
  | ValueBinder (x, y, z) ->
      Printf.sprintf "let %s = %s in %s" x (source y) (source z)

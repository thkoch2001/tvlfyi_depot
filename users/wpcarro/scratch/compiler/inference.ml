(*******************************************************************************
 * WIP implementation of the Hindley-Milner type system primarily for learning
 * purposes.
 *
 * Wish List:
 * - TODO Debug this inference (let f (fn x x) f)
 ******************************************************************************)

open Types
open Debug

(*******************************************************************************
 * Library
 ******************************************************************************)

let ( let* ) = Option.bind

let set_from_list (xs : string list) : set =
  xs |> List.fold_left (fun acc x -> FromString.add x true acc) FromString.empty

(* Map union that favors the rhs values (i.e. "last writer wins"). *)
let lww (xs : 'a FromString.t) (ys : 'a FromString.t) : 'a FromString.t =
  FromString.union (fun k x y -> Some y) xs ys

let emptyEnv : env = FromString.empty

let rec free_type_vars (t : _type) : set =
  match t with
  | TypeVariable k -> FromString.singleton k true
  | TypeInt -> FromString.empty
  | TypeBool -> FromString.empty
  | TypeArrow (a, b) -> lww (free_type_vars a) (free_type_vars b)

let i : int ref = ref 0

let make_type_var () : _type =
  let res = Printf.sprintf "a%d" !i in
  i := !i + 1;
  TypeVariable res

exception OccursCheck

let bind_var (k : string) (t : _type) : substitution =
  if t == TypeVariable k then FromString.empty
  else if FromString.exists (fun name _ -> name == k) (free_type_vars t) then
    raise OccursCheck
  else FromString.singleton k t

let rec instantiate (q : quantified_type) : _type =
  let (QuantifiedType (names, t)) = q in
  match t with
  | TypeInt -> TypeInt
  | TypeBool -> TypeBool
  | TypeVariable k ->
      if List.exists (( == ) k) names then make_type_var () else TypeVariable k
  | TypeArrow (a, b) ->
      TypeArrow
        (instantiate (QuantifiedType (names, a)), instantiate (QuantifiedType (names, b)))

let quantified_type_ftvs (q : quantified_type) : set =
  let (QuantifiedType (names, t)) = q in
  lww (free_type_vars t) (names |> set_from_list)

let generalize (env : env) (t : _type) : quantified_type =
  let envftv =
    env |> FromString.bindings
    |> List.map (fun (_, v) -> quantified_type_ftvs v)
    |> List.fold_left lww FromString.empty
  in
  let names =
    lww (free_type_vars t) envftv
    |> FromString.bindings
    |> List.map (fun (k, _) -> k)
  in
  QuantifiedType (names, t)

let rec substitute_type (s : substitution) (t : _type) : _type =
  match t with
  | TypeVariable k as tvar ->
     (match FromString.find_opt k s with
      | Some v -> substitute_type s v
      | None -> tvar)
  | TypeArrow (a, b) -> TypeArrow (substitute_type s a, substitute_type s b)
  | TypeInt -> TypeInt
  | TypeBool -> TypeBool

let substitute_quantified_type (s : substitution) (q : quantified_type) : quantified_type =
  let (QuantifiedType (names, t)) = q in
  let s1 =
    FromString.filter (fun k v -> List.exists (fun x -> k != x) names) s
  in
  QuantifiedType (names, substitute_type s1 t)

let substitute_env (s : substitution) (env : env) : env =
  FromString.map (fun q -> substitute_quantified_type s q) env

let compose_substitutions (xs : substitution list) : substitution =
  let do_compose_substitutions s1 s2 = lww s2 (FromString.map (substitute_type s2) s1) in
  List.fold_left do_compose_substitutions FromString.empty xs

let rec unify (a : _type) (b : _type) : substitution option =
  match (a, b) with
  | TypeInt, TypeInt -> Some FromString.empty
  | TypeBool, TypeBool -> Some FromString.empty
  | TypeVariable k, _ -> Some (bind_var k b)
  | _, TypeVariable k -> Some (bind_var k a)
  | TypeArrow (a, b), TypeArrow (c, d) ->
      let* s1 = unify a c in
      let* s2 = unify (substitute_type s1 b) (substitute_type s1 d) in
      let s3 = compose_substitutions [s1; s2] in
      s1 |> Debug.substitution |> Printf.sprintf "s1: %s\n" |> print_string;
      s2 |> Debug.substitution |> Printf.sprintf "s2: %s\n" |> print_string;
      s3 |> Debug.substitution |> Printf.sprintf "s3: %s\n" |> print_string;
      Some s3
  | _ -> None

let print_env (env : env) =
  Printf.sprintf "env: %s\n" (Debug.env env)
  |> print_string

let print_val (x : value) =
  Printf.sprintf "val: %s\n" (Debug.value x)
  |> print_string

let print_inference (x : inference option) =
  match x with
  | None -> "no inference\n" |> print_string
  | Some x ->
     Printf.sprintf "inf: %s\n" (Debug.inference x)
     |> print_string

let rec infer (env : env) (x : value) : inference option =
  print_env env;
  print_val x;
  let res = match x with
  | ValueLiteral lit -> (
      match lit with
      | LiteralInt _ -> Some (Inference (FromString.empty, TypeInt))
      | LiteralBool _ -> Some (Inference (FromString.empty, TypeBool)))
  | ValueVariable k ->
      let* v = FromString.find_opt k env in
      Some (Inference (FromString.empty, instantiate v))
  | ValueFunction (param, body) ->
      let typevar = make_type_var () in
      let env1 = FromString.remove param env in
      let env2 = lww (FromString.singleton param (QuantifiedType ([], typevar))) env1 in
      let* (Inference (s1, t1)) = infer env2 body in
      Some (Inference (s1, TypeArrow (substitute_type s1 typevar, t1)))
  | ValueApplication (f, x) ->
      let result = make_type_var () in
      let* (Inference (s1, t1)) = infer env f in
      let* (Inference (s2, t2)) = infer (substitute_env s1 env) x in
      let* s3 = unify (substitute_type s2 t1) (TypeArrow (t2, result)) in
      Some (Inference
              ( compose_substitutions [s3; s2; s1],
                substitute_type s3 result ))
  | ValueVarApplication (name, x) ->
      let* v = FromString.find_opt name env in
      let t1 = instantiate v in
      let typevar = make_type_var () in
      let* (Inference (s2, t2)) = infer env x in
      let* s3 = unify (substitute_type s2 t1) (TypeArrow (t2, typevar)) in
      Some (Inference
              ( compose_substitutions [s2; s3],
                substitute_type s3 typevar ))
  | ValueBinder (k, v, body) ->
      let* (Inference (s1, t1)) = infer env v in
      let env1 = FromString.remove k env in
      let tg = generalize (substitute_env s1 env) t1 in
      let env2 = FromString.add k tg env1 in
      let* (Inference (s2, t2)) = infer (substitute_env s1 env2) body in
      Some (Inference (compose_substitutions [s1; s2], t2)) in
  print_inference res;
  res

let do_infer (x : value) : _type option =
  let* Inference (_, t) = infer FromString.empty x in
  Some t

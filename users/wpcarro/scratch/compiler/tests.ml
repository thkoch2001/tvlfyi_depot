open Expr_parser
open Type_parser
open Inference

type test = { input : string; expect : string }
(* type sub_test = { s1 : string; s2 : string; s3 : string } *)

let ( let* ) = Option.bind

let tests =
  [
    { input = "((fn x x) 10)"; expect = "Integer" };
    { input = "(let f (fn x x) f)"; expect = "a -> a" };
  ]

(* let sub_tests = [ *)
(*     { *)
(*       s1 = "{b |-> b -> Int}"; *)
(*       s2 = "{a: Bool, b: Int, c: Bool}"; *)
(*       s3 = "{a: Bool, b: Int -> Int, c: Bool}"; *)
(*     } *)
(* ] *)

exception FailedAssertion
exception TestError

let main =
  tests
  |> List.iter (fun { input; expect } ->
         Printf.sprintf ":t %s == %s\n" input expect |> print_string;
         match (parse_language input, parse_input expect) with
         | Some ast, Some expected -> (
             match do_infer ast with
             | Some actual ->
                 if actual != expected then (
                   print_type actual;
                   raise FailedAssertion)
                 else print_string "Test passed.\n"
             | _ -> raise TestError)
         | _ -> raise TestError);
  print_string "All tests pass!"

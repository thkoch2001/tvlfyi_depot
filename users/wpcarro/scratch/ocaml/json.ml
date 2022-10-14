(*******************************************************************************
 * Some experiments to get familiar with JSON in OCaml...
 *
 * Usage:
 *   $ utop
 *   utop # #require "yojson";;
 *   utop # #use "json.ml";;
 ******************************************************************************)

(******************************************************************************
 * Types
 ******************************************************************************)

type color = Red | Orange | Green | Blue | Indigo | Violet

type person = {
    fname : string;
    lname : string;
    age: int;
    fav_color : color [@key "color"];
  } [@@deriving yojson_of];;

(******************************************************************************
 * Functions
 ******************************************************************************)

(* Unprocessed JSON as a string *)
let one : string = {|{"fname":"William","lname":"Carroll","age":30}|}

(* Parse into a loosely typed representation *)
let two =
  let json = {|{"fname":"William","lname":"Carroll","age":30}|} in
  let parsed = Yojson.Safe.from_string json in
  let open Yojson.Safe.Util in
  parsed |> to_assoc |> List.remove_assoc "fname" |> List.cons ("fname", `String "Bill")

let parse_color (x : string) : color option =
  match x with
  | "red" -> Some Red
  | "orange" -> Some Orange
  | "green" -> Some Green
  | "blue" -> Some Blue
  | "indigo" -> Some Indigo
  | "violet" -> Some Violet
  | _ -> None

(* Manually parse into a strongly typed representation *)
let three : person option =
  let json = {|{"fname":"William","lname":"Carroll","age":30,"color":"red"}|} in
  let parsed = Yojson.Safe.from_string json in
  let open Yojson.Safe.Util in
  match (parsed |> member "fname",
         parsed |> member "lname",
         parsed |> member "age",
         parsed |> member "color" |> to_string |> parse_color) with
  | (`String x, `String y, `Int z, Some c) ->
     Some {fname = x; lname = y; age = z; fav_color = c;}
  | _ -> None

(* Automatically parse into a strongly typed representation *)
let three : person =
  let json = {|{"fname":"William","lname":"Carroll","age":30,"color":"red"}|} in
  yojson_of_person json

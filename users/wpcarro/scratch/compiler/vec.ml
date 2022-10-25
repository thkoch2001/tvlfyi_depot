(****************************************************************************** 
 * Similar to Python's list
 *
 * - mutable
 * - dynamically resized
 * - O(1) read
 * - O(1) write
 * - O(1) append (average case)
 *
 ******************************************************************************)

type 'a vec = {
  mutable length: int;
  mutable capacity: int;
  mutable xs: 'a array;
}

(****************************************************************************** 
 * Constructors
 ******************************************************************************)

let make (size : int) (seed : 'a) : 'a vec = { 
  length = size;
  capacity = size;
  xs = Array.make size seed;
}

let create () = {
  length = 0;
  capacity = 0;
  xs = [||];
}

let from_array (xs : 'a array) : 'a vec = {
  length = Array.length xs;
  capacity = Array.length xs;
  xs = xs;
}

let from_list (xs : 'a list) : 'a vec = 
  match xs with
  | [] -> create ()
  | y::ys -> 
    let result = {
      length = List.length xs;
      capacity = List.length xs;
      xs = Array.make (List.length xs) y;
    } in
    List.iteri (fun i x -> Array.set result.xs i x) xs;
    result

(****************************************************************************** 
 * Miscellaneous
 ******************************************************************************)

let append (x : 'a) (v : 'a vec) =
  if v.capacity = 0 then
    begin
      v.length <- 1;
      v.capacity <- 1;
      v.xs <- [|x|];
    end
  else if v.length = v.capacity then
    begin
      (* According to Wikipedia, Python uses 1.25 as the growth factor *)
      let new_cap = v.capacity |> float_of_int |> Float.mul 1.25 |> ceil |> int_of_float in
      let new_xs = Array.make new_cap x in
      Array.iteri (fun i x -> Array.set new_xs i x) v.xs;
      v.capacity <- new_cap;
      v.xs <- new_xs;
      Array.set v.xs v.length x;
      v.length <- v.length + 1;
    end
  else
    begin
      Array.set v.xs v.length x;
      v.length <- v.length + 1;
    end

let get (i : int) (v : 'a vec) : 'a option =
  if i >= v.length then
    None
  else
    Some v.xs.(i)

let get_unsafe (i : int) (v : 'a vec) : 'a =
  v.xs.(i)

let set (i : int) (x : 'a) (v : 'a vec) : unit =
  if i < v.length then
    Array.set v.xs i x

let length (v : 'a vec) : int = 
  v.length

let update (i : int) (f : 'a -> 'a) (v : 'a vec) : unit =
  match get i v with
  | None -> ()
  | Some x -> set i (f x) v

let iter (f : 'a -> unit) (v : 'a vec) : unit =
  let n = ref 0 in
  while !n < v.length do
    f v.xs.(!n);
    n := !n + 1;
  done

let join (sep : string) (v : string vec) : string =
  if length v = 0 then
    ""
  else
    let i = ref 1 in
    let result = ref v.xs.(0) in
    while !i < v.length do
      result := !result ^ sep ^ v.xs.(!i);
      i := !i + 1;
    done;
    !result

let map (f : 'a -> 'b) (v : 'a vec) : 'b vec =
  let result = create () in
  iter (fun x -> append (f x) result) v;
  result

let append_to (dst : 'a vec) (xs : 'a vec) : unit =
  iter (fun x -> append x dst) xs


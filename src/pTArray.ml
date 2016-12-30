type 'a t = {
  decal : int;
  roots : 'a list;
  subtrees : 'a t list;
}
(* - There are [order - 1] root elements
 * - There are [order] subtrees *)

let fast_exp x e =
  let rec aux acc x e =
    if e = 0 then acc
    else if e mod 2 = 0 then aux acc (x * x) (e / 2)
    else aux (x * acc) x (e - 1)
  in
  if e < 0 then failwith "Can't use fast_exp on a negative integer"
  else aux 1 x e

(* Gives the maximum number of elements an UBTree of given order and height can
 * contain.
 *
 * The UBTree can contain at most : [(1 + order + order^2 + ...  +
 * order^(height-1)) * (order - 1)] elements. Call this formula (1).
 *
 * The subformula [1 + order + order^2 + ... + order^height] is a well known
 * geometric series which is equal to [(order^(height+1) - 1) / (order - 1)].
 *
 * Hence the whole formula (1) is equal to [order^(height+1) - 1]. *)
let max_elements order height = fast_exp order height - 1

let clog c n = log n /. log c

(* Gives the minimum height such that an UBTree of given order of this height
 * can hold a given number of elements.
 *
 * The formula is obtained by reversing the formula from the [max_elements]
 * function. *)
let min_height order elements =
  int_of_float (ceil (clog (float order) (float elements +. 1.)))

let cut n l =
  let rec aux (acc, l) = function
    | 0 -> (List.rev acc, l)
    | i -> match l with
      | [] -> (acc, l)
      | h :: t -> aux (h :: acc, t) (i - 1)
  in aux ([], l) n

let choose_order n =
  let rec aux cur =
    if min_height cur n > cur then aux (cur + 1)
    else cur
  in aux 2

let of_list l =
  let len = List.length l in
  let order = choose_order len in
  let h = min_height order len in
  let rec make_root decal level l =
    let roots, l = cut (order - 1) l in
    let subtrees, decal', l = make_subtrees order (level + 1) (decal + order - 1) l in
    ({ decal; roots; subtrees }, decal', l)
  and make_subtrees left level decal l = match l with
  | l when l = [] || left = 0 || level = h -> ([], decal, l)
  | l ->
      let st, decal, l = make_root decal level l in
      let (sts, decal, l) = make_subtrees (left - 1) level decal l in
      (st :: sts, decal, l)
  in let r, _, _ = make_root 0 0 l in r

let rec to_list pta =
  pta.roots @ (List.concat (List.map to_list pta.subtrees))

let init_list f n = 
  let rec aux acc = function
    | i when i < 0 -> acc
    | i -> aux (f i :: acc) (i - 1)
  in aux [] (n - 1)

let init n f = of_list (init_list f n)

let of_array a = of_list (Array.to_list a)

let make n v = init n (fun _ -> v)

let rec last = function
  | [] -> assert false
  | h :: [] -> h
  | h :: t -> last t

let rec length = function
  | x when x.subtrees = [] -> x.decal + List.length x.roots
  | x -> length (last x.subtrees)

let rec find_subtree i cur = function
  | [] -> cur
  | h :: t when h.decal > i -> cur
  | h :: t -> find_subtree i h t

let rec get_list i = function
  | [] -> raise Not_found
  | h :: t when i = 0 -> h
  | h :: t -> get_list (i - 1) t

let rec get a i =
  let st = find_subtree i a a.subtrees in
  if st = a then get_list (i - a.decal) a.roots
  else get st i

let rec set_list i v = function
  | [] -> raise Not_found
  | h :: t when i = 0 -> v :: t
  | h :: t -> h :: set_list (i - 1) v t

let rec set a i v =
  if a.subtrees = [] || (List.hd a.subtrees).decal > i
  then { a with roots = set_list (i - a.decal) v a.roots }
  else { a with subtrees = update_subtrees a.subtrees i v }
and update_subtrees l i v = match l with
| [] -> assert false
| h :: [] -> set h i v :: []
| h :: h' :: t when h'.decal > i -> set h i v :: h' :: t
| h :: t -> h :: update_subtrees t i v

let rec list_iteri f start = function
  | [] -> ()
  | h :: t -> f start h; list_iteri f (start + 1) t

let rec iteri f a =
  list_iteri f a.decal a.roots;
  List.iter (iteri f) a.subtrees

let rec iter f a =
  List.iter f a.roots;
  List.iter (iter f) a.subtrees

let rec iter2 f a b =
  List.iter2 f a.roots b.roots;
  List.iter2 (iter2 f) a.subtrees b.subtrees

let to_array a =
  let len = length a in
  if len = 0 then [| |]
  else
    let arr = Array.make len (List.hd a.roots) in
    iteri (fun i x -> arr.(i) <- x) a;
    arr

let rec list_mapi f start = function
  | [] -> []
  | h :: t -> f start h :: list_mapi f (start + 1) t

let rec map f a =
  { a with roots = List.map f a.roots; subtrees = List.map (map f) a.subtrees }

let rec mapi f a =
  { a with roots = list_mapi f a.decal a.roots; subtrees = List.map (mapi f) a.subtrees }

let rec map2 f a b =
  { a with roots = List.map2 f a.roots b.roots; subtrees = List.map2 (map2 f) a.subtrees b.subtrees }

let rec print ?(prefix="") f pt =
  print_string prefix;
  List.iter (fun x -> Printf.printf "%s " (f x)) pt.roots;
  print_newline ();
  List.iter (print ~prefix:(prefix^"  ") f) pt.subtrees

let rec fold_left f acc a =
  let acc = List.fold_left f acc a.roots in
  List.fold_left (fold_left f) acc a.subtrees

let rec list_foldi_left f acc start = function
  | [] -> acc
  | h :: t -> list_foldi_left f (f acc start h) (start + 1) t

let rec foldi_left f acc a =
  let acc = list_foldi_left f acc a.decal a.roots in
  List.fold_left (foldi_left f) acc a.subtrees

let rec fold_left2 f acc a b =
  let acc = List.fold_left2 f acc a.roots b.roots in
  List.fold_left2 (fold_left2 f) acc a.subtrees b.subtrees

let rec mem a v =
  List.mem v a.roots || List.exists (fun st -> mem st v) a.subtrees

let rec memq a v =
  List.memq v a.roots || List.exists (fun st -> memq st v) a.subtrees

let rec for_all p a =
  List.for_all p a.roots && List.for_all (for_all p) a.subtrees

let rec list_for_alli p start = function
  | [] -> true
  | h :: t -> p start h && list_for_alli p (start + 1) t

let rec for_alli p a =
  list_for_alli p a.decal a.roots && List.for_all (for_alli p) a.subtrees

let rec exists p a =
  List.exists p a.roots || List.exists (exists p) a.subtrees

let rec list_existsi p start = function
  | [] -> false
  | h :: t -> p start h || list_existsi p (start + 1) t

let rec existsi p a =
  list_existsi p a.decal a.roots || List.exists (existsi p) a.subtrees

let rec fold_right f a v =
  let v = List.fold_right (fold_right f) a.subtrees v in
  List.fold_right f a.roots v

let rec list_foldi_right f start l v = match l with
| [] -> v
| h :: t -> f start h (list_foldi_right f (start + 1) t v)

let rec foldi_right f a v =
  let v = List.fold_right (foldi_right f) a.subtrees v in
  list_foldi_right f a.decal a.roots v











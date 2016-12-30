(**
 * A persistent tree-array is an immutable array internally represented as a
 * tree whose each non-leaf node has k children (except maybe one). Each node
 * contains k - 1 of the array.
 *
 * The order k is automatically chosen in such a way that the height of the
 * tree representation is similar to k. Note that k = 2 (resulting in a binary
 * tree) only for arrays with at most 3 values, and then that k = 3 only up to
 * 26 values. On the other hand with k = 10 one can store up to 10^10-1 values.
 *
 * The main advantages of persistent tree-arrays are that
 * 1) They are persistent, which means when you (functionnaly) update some
 *    value, the older version is still available;
 * 2) Updating and keeping a copy is done in logarithmic time and space
 *    complexity, as opposed as with standard arrays, where these complexities
 *    are linear;
 * 3) Values can be still be retrieved by index;
 * 4) Functionals (map, iter and their derivatives) are available and done in
 *    linear time and space complexities.
 *
 * This comes at the cost of logarithmic single value retrieve and update.
 *
 * One use case that comes to mind is the implementation of alpha-beta AIs,
 * where updating the state while keeping older versions is important and
 * should be done as fast as possible. This notably allows parallelism of
 * computations.
 * *)
type 'a t

val length : 'a t -> int
(** [length pta] O(k * k)
 * returns the number of elements [pta] contains. *)

val get : 'a t -> int -> 'a
(** [get pta i] O(k * k)
 * returns the element of [pta] at position [i]. *)

val set : 'a t -> int -> 'a -> 'a t
(** [set pta i v] O(k * k)
 * returns a new persistent tree-array in which value at position [i] has been
 * replaced with [v], while other elements remain unchanged. *)

val to_list : 'a t -> 'a list
(** [to_list pta] O(length pta)
 * Transforms a persistent tree-array into a list. *)

val to_array : 'a t -> 'a array
(** [to_array pta] O(length pta)
 * Transforms a persistent tree-array into an array. *)

(* {2 Creation} *)

val make : int -> 'a -> 'a t
(** [make n v] O(n)
 * creates a new persistent tree-array with [n] elements, all initialised to
 * [v]. *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] O(n)
 * Initializes a persistent tree-array with [n] elements using function [f].
 * If [n] is negative, returns an empty array. *)

val of_list : 'a list -> 'a t
(** [of_list l] O(List.length n)
 * Transforms a list into a persistent tree-array. *)

val of_array : 'a array -> 'a t
(** [of_array a] O(Array.length a)
 * Transforms an array into a persistent array. *)

(* {2 Iterators} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f pta] O(length pta * complexity of f)
 * Applies [f] to each element of [pta], in order. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f pta] O(length pta * complexity of f)
 * Applies [f] to each element of [pta] with its index, in order. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f pta] O(length pta * complexity of f)
 * Produces a new persistent tree-array in which each element [x] is replaced
 * with [f x]. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi f pta] O(length pta * complexity of f)
 * Produces a new persistent tree-array in which each element [x] at position
 * [i] is replaced with [f i x]. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold_left f start pta] O(length pta * complexity of f)
 * Folds the persistent tree-array v0 v1 ... vn in the following fashion:
 *   f (... (f (f start v0) v1) ...) vn. *)

val foldi_left : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [foldi_left f start pta]
 * Same as [fold_left] but the index is also given as a parameter to the
 * folding function. *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold_right f pta start] O(length pta * complexity of f)
 * Folds the persistent tree-array v0 v1 ... vn in the following fashion:
 *   f v0 (f v1 (... (f vn start) ...)).
 * Be aware that this function is not tail-recursive. *)

val foldi_right : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [foldi_right f pta start]
 * Same as [fold_right] but the index is also given as a parameter to the
 * folding function. *)

(* {2 Iterators on two persistent tree-arrays} *)

exception Lengths_differ

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f pta pta'] O(length pta * complexity of f)
 * Same as [iter] but on two persistent tree-arrays.
 * @throws Lengths_differ if [length pta <> length pta']. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f pta pta'] O(length pta * complexity of f)
 * Same as [map] but on two persistent tree-arrays.
 * @throws Lengths_differ if [length pta <> length pta']. *)

val fold_left2 : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
(** [fold_left2 f acc pta pta'] O(length pta * complexity of f)
 * Same as [fold_left] but on two persistent tree-arrays.
 * @throws Lengths_differ if [length pta <> length pta']. *)

val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
(** [fold_right2 f pta pta' v] O(length pta * complexity of f)
 * Same as [fold_right] but on two persistent tree-arrays.
 * @throws Lengths_differ if [length pta <> length pta']. *)

(* {2 Scanning} *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p pta] O(length pta * complexity of p)
 * Checks that every value of [pta] satisfies the predicate [p]. *)

val for_alli : (int -> 'a -> bool) -> 'a t -> bool
(** [for_alli p pta] O(length pta * complexity of p)
 * Checks that every value of [pta] satisfies the predicate [p] given its
 * position. *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p pta] O(length pta * complexity of p)
 * Checks that at least one value in [pta] satisfies the predicate [p]. *)

val existsi : (int -> 'a -> bool) -> 'a t -> bool
(** [existsi p pta] O(length pta * complexity of p)
 * Checks that at least one value in [pta] satisfies the predicate [p] given
 * its position. *)

val mem : 'a t -> 'a -> bool
(** [mem pta v] O(length pta)
 * Checks whether [v] is a member of [pta]. *)

val memq : 'a t -> 'a -> bool
(** [memq pta v] O(length pta)
 * Checks whether [v] is a member of [pta] using physical identity. *)

val find : ('a -> bool) -> 'a t -> 'a
(** [find p pta] O(length pta * complexity of p)
 * Finds and returns the first element in [pta] that satisfies [p].
 * @throws Not_found if not found. *)

val findi : (int -> 'a -> bool) -> 'a t -> int * 'a
(** [findi p pta] O(length pta * complexity of p)
 * Finds and returns the first element in [pta] that satisfies [p] along with
 * its index, which [p] has also access to.
 * @throws Not_found if not found. *)

val find_all : ('a -> bool) -> 'a t -> 'a list
(** [find_all p pta] O(length pta * complexity of p)
 * Finds and returns all the elements of [pta] that satisfy [p], in their order
 * of appearance in [pta]. *)

val findi_all : (int -> 'a -> bool) -> 'a t -> (int * 'a) list
(** [findi_all p pta] O(length pta * complexity of p)
 * Finds and returns all the elements of [pta] that satisfy [p] along with
 * their indices, which [p] has also access to, in their order of appearance in
 * [pta]. *)


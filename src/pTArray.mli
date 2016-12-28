(**
 * A persistent tree-array is an immutable array internally represented as a
 * tree whose each non-leaf node has k children (except maybe one). Each node
 * contains k - 1 of the array.
 *
 * The order k is automatically chosen in such a way that the height of the
 * tree representation is similar to k. Note that k = 2 (resulting in a binary
 * tree) only for arrays with at most 3 values, and then that k = 3 only up to
 * 26 values.
 *
 * Every use of logarithm in the present documentation is to be read as
 * "logarithm in base k".
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
(** [length pta] O(log(length pta) * log(length pta))
 * returns the number of elements [pta] contains. *)

val get : 'a t -> int -> 'a
(** [get pta i] O(log(length pta))
 * returns the element of [pta] at position [i]. *)

val set : 'a t -> int -> 'a -> 'a t
(** [set pta i v] O(log(lenpth pta))
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

(* {2 Iterators on two persistent tree-arrays} *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val fold_left2 : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c

(*

val fold_right
val foldi_right
val fold2_right

val for_all
val exists

val mem : 'a t -> 'a -> bool
val memq : 'a t -> 'a -> bool

val find : 'a t -> ('a -> bool) -> 'a
val findi : 'a t -> (int -> 'a -> bool) -> int * 'a

val find_all : 'a t -> ('a -> bool) -> 'a list
val findi_all : 'a t -> (int -> 'a -> bool) -> (int * 'a) list
*)

val print : ?prefix:string -> ('a -> string) -> 'a t -> unit

(* TODO add lwt support *)


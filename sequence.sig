signature SEQUENCE =
sig
  type 'a seq

  (* Provides a view of the abstract 'a seq type as a binary tree. *)
  datatype 'a treeview = EMPTY
                       | ELT of 'a
                       | NODE of ('a seq * 'a seq)

  (* Provides a view of the abstract 'a seq type as a list. *)
  datatype 'a listview = NIL
                       | CONS of ('a * 'a seq)

  (* Ordering on the element type 'a *)
  type 'a ord = 'a * 'a -> order

  val empty : unit -> 'a seq

  val singleton : 'a -> 'a seq

  val length : 'a seq -> int

  (*
    @params s n
    @param s a sequence
    @param n an index into the sequence
    @returns the nth element of s or raises Subscript if n is not a valid
    index
  *)
  val nth : 'a seq -> int -> 'a

  (*
    @params f n
    @param f a function taking a sequence index to the value to go at that
      sequence
    @param n the length of the output sequence
    @returns a sequence of length n where n_i = f(i)
  *)
  val tabulate : (int -> 'a) -> int -> 'a seq

  (*
    @params l
    @param l an 'a list
    @returns an 'a seq logically equivalent
      tabulate (fn i => List.nth(l,i)) (List.length l)
  *)
  val fromList : 'a list -> 'a seq


  (*
    @params ord
    @param ord an ordering on 'a
    @returns an ordering on 'a seqs derived lexigographically from ord
  *)
  val collate : 'a ord -> 'a seq ord

  (*
    @params f s
    @param f the function ('a -> 'b) to apply to each element of s
    @param s an 'a sequence
    @returns a 'b sequence whose ith element is f(s_i)
  *)
  val map : ('a -> 'b) -> 'a seq -> 'b seq

  (*
    @params f s1 s2
    @param f a function ('a * 'b -> 'c) to apply to the elements of s1 and
      s2
    @param s1 an 'a sequence of length n
    @param s2 a 'b sequence of length n
    @returns a 'c sequence of length n whose ith element is f(s1_i, s2_i)
  *)
  val map2 : (('a * 'b) -> 'c) -> 'a seq -> 'b seq -> 'c seq

  (* val mapprod : (('a * 'b) -> 'c) -> 'a seq -> 'b seq -> 'c seq *)
  (* val mapsum : (('a * 'b) -> 'c) -> 'a seq -> 'b seq -> 'c seq *)

  (* Parallel fold (XXX say more)
    @params f b s
    @param f a combiner
    @param b a base case
    @param s the sequence to reduce
  *)
  val reduce : (('a * 'a) -> 'a) -> 'a -> 'a seq -> 'a

  (*
    Like reduce but returns intermediate values.
    @params f b s
    @param f a combiner
    @param b a base case
    @param s the sequence to scan
  *)
  val scan : (('a * 'a) -> 'a) -> 'a -> 'a seq -> ('a seq * 'a)

  (*
    @params p s
    @param p a filter condition
    @param s a sequence
    @returns s without the elements that don't satisfy p.
  *)
  val filter : ('a -> bool) -> 'a seq -> 'a seq

  (*
    @params f b s
    @param f a combiner
    @param b a base case
    @param s the sequence to iterate over
  *)
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a seq -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a seq -> 'b

  (*
    @params f b s
    @returns all partial results of foldl
  *)
  val foldlh : ('a * 'b -> 'b) -> 'b -> 'a seq -> ('b seq * 'b)

  (*
    Logically equivalent to "reduce append (empty ())"
    @params s
    @param s a sequence of sequences
    @returns a flattened sequence
  *)
  val flatten : 'a seq seq -> 'a seq

  (*
    @params I s
    @param I a sequence of indices into s
    @param s a sequence
    @returns a sequence of adjacent subsequences of s with lengths
      specified by the elements of I.
  *)
  val partition : int seq -> 'a seq -> 'a seq seq

  (* 
    @params ind s
    @param ind a sequence of pairs (i,x) where i is an index into s
    @param s a sequence
    @returns s where s_i is updated to x when (i,x) is in ind. If multiple
     pairs with i as the first element exist, later values are preferred.
  *)
  val inject : (int*'a) seq -> 'a seq -> 'a seq

  (*
    @params s1 s2
    @returns s1@s2
  *)
  val append : 'a seq * 'a seq -> 'a seq

  (*
    @params p
    @param p a pair (s, i) of a sequence and index into that sequence
    @returns the subsequence of s from indices 0 to i
    @exception Subscript raised if i is out of bounds
  *)
  val take : 'a seq * int -> 'a seq

  (*
    @params p
    @param p a pair (s, i) of a sequence and index into that sequence
    @returns 
  *)
  val drop : 'a seq * int -> 'a seq

  (*
    @params s tr
    @param s a sequence
    @param tr a triple (start, end, step)
    @returns a subsequence s' of s such that s'_i = s_{start+i*step}
  *)
  val rake : 'a seq -> (int * int * int) -> 'a seq

  (*
    @params s range
    @params s a seq
    @params range = (start, len) initial index and length of the subsequence
    @returns a subsequence s' of s from index start to start+len
    @throws Subscript if start or start+len is an invalid index
  *)
  val subseq : 'a seq -> (int * int) -> 'a seq

  (* Splits a sequence down the middle. *)
  val splitMid : 'a seq * int -> ('a seq * 'a * 'a seq) option

  (*
    @params ord s
    @param ord an ordering
    @param s a sequence
    @returns a rearrangement of the elements of s s.t. s is sorted with
    respect to ord.
   %% TODO: is this well def with any ordering? in place, stable?
  *)
  val sort : 'a ord -> 'a seq -> 'a seq

  (* 
    @params ord s1 s2
    @returns an interleaving of s1 and s2 according to ord
   *)
  val merge : 'a ord -> 'a seq -> 'a seq -> 'a seq

  (* 
    @params ord seq
    @param ord an ordering
    @param s a sequence of pairs (x, y)
    @returns a sorted sequence of pairs (x, ys) where each x only occurs
      once and ys is all values y s.t. (x, y) is in s
  *)
  val collect : 'a ord -> ('a * 'b) seq -> ('a * 'b seq) seq

  (*
    @params f s
    @param f a function from elements of s to strings
    @param s a sequence
    @returns the string "<str_1, ..., str_n>" where str_i = f(s_i)
  *)
  val toString : ('a -> string) -> 'a seq -> string

  (*
    @params p str
    @param p a delimiter predicate (if p(x) then x is a delim char)
    @param str a string
    @returns the tokens of str (stripping out delimiters)
  *)
  val tokens : (char -> bool) -> string -> string seq


  (*
    @params p str
    @param p a delimiter predicate
    @param str a string
    @returns the fields of str (including delimiters)
  *)
  val fields : (char -> bool) -> string -> string seq

  (*
    @params s
    @param s a sequence
    @returns EMPTY if s is the empty sequence
             ELT a if s is a sequence containing one element, a
             NODE (s1, s2) if s consists of two sequences (of > 0 size)
              such that s1 = take(s,|s|/2) and s2 = drop (s, |s|/2)
  *)
  val showt : 'a seq -> 'a treeview

  (* Like showt but performs the split based on a supplied index.
    @params s f
    @param s a sequence
    @param f a function of the size of s
    @returns the same as above for showt except that in the NODE(s1,s2)
      case, s1 = take(s,f(|s|)) and s2 = drop(s, f|s|)
   *)
  val showti : 'a seq -> (int -> int) -> 'a treeview

  (* 
    @params t
    @param t a treeview
    @returns if tv is EMPTY, then <>
             if tv is (ELT x) then <x>
             if tv is NODE(l,r) then l@r
   *)
  val hidet : 'a treeview -> 'a seq

  (* 
    @params s
    @param s a sequence
    @returns if |s| = 0 then NIL
             if |s| > 0 then CONS(s_0,<s_1,...,s_n>)
   *)
  val showl : 'a seq -> 'a listview

  (* 
    @params l
    @param l a listview
    @returns if lv is NIL, then <>
             if lv is CONS(x,xs) then <x>@xs
   *)
  val hidel : 'a listview -> 'a seq

  val toList : 'a seq -> 'a list
  val cons : 'a * 'a seq -> 'a seq



  (**** Deprecated ****)

  val iter : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b
  val iterh : ('b * 'a -> 'b) -> 'b -> 'a seq -> ('b seq * 'b)
  

end

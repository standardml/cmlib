(* Based on 210/PRIORITY_QUEUE.sig *)

signature PQUEUE =
sig

  (* This is the abstract type for priority queues. *)
  structure Key : ORDERED

  (* This indicates that the type of keys in a priority queue has to have type key. *)
  type key = Key.t

  (* This is the abstract type representing a priority queue with key type
   * key (see below) and value type 'a. 
   *)
  type 'a pq
  type 'a t = 'a pq

  (* empty represents the empty collection. *)
  val empty : unit -> 'a pq

  (* Returns true if the priority queue is empty. *)
  val isEmpty : 'a pq -> bool

  (* If k is a value of type key and v is a  value of type 'a, the expression 
   * singleton (k,v) evaluates to the priority queue including just (k,v).
   *)
  val singleton : key * 'a -> 'a pq

  (* For a a key-value pair (k, v), and a priority queue Q,
   * insert (k, v) Q evaluates to Q Union {(k, v)}.  Since the
   * priority queue is treated as a multiset, duplicate keys or key-value
   * pairs are allowed and kept separately.
   *)
  val insert : (key*'a) -> 'a pq -> 'a pq

  (* Takes the union of two priority queues.  Since the priority queue
   * is treated as a multiset, duplicate keys or key-value pairs are 
   * allowed and kept.  Therefore the size of the result will be the sum
   * of the sizes of the inputs.
   *)
  val meld      : 'a pq -> 'a pq -> 'a pq

  (* Given a priority queue findMin Q, if Q is empty it returns 
   * NONE.   Otherwise it returns SOME(k,v) where (k,v) in Q 
   * and $k$ is the key of minimum value in $Q$.  If multiple elements 
   * have the same minimum valued key, then an arbitrary one is returned. 
   *)
  val findMin   : 'a pq -> (key*'a) option

  (* This is the same as findMin but also returns a priority queue
   * with the returned (key,value) pair removed (if the input queue is
   * non-empty) or an empty Q (if the input queue is empty).
   *)
  val deleteMin : 'a pq -> (key*'a) option * 'a pq
end

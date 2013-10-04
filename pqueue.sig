(* Originally based on 210/PRIORITY_QUEUE.sig, since modified to follow cmlib/queue.sig *)

signature PQUEUE =
sig

  structure Key : ORDERED                          (* contains key type t along with an ordering *)
  type key = Key.t                         (* the type of keys used as priorities in the ipqueue *)

  type 'a pqueue        (* priority queues with the above key type and value type 'a *)
  type 'a t = 'a pqueue

  exception Empty                                       (* raised by findMin and deleteMin below *)
  val empty     : unit -> 'a pqueue                           (* creates an empty priority queue *)
  val singleton : key * 'a -> 'a pqueue          (* creates a priority queue with one (key, val) *)

  val isEmpty   : 'a pqueue -> bool               (* returns true if the priority queue is empty *)

  val insert    : 'a pqueue -> (key*'a) -> 'a pqueue                        (* functional insert *)
  val meld      : 'a pqueue -> 'a pqueue -> 'a pqueue                (* copy two queues into one *)

  val findMin   : 'a pqueue -> (key*'a) option  (* return SOME (k,v) with min k in a pqueue NONE *)
  val lookupMin : 'a pqueue -> (key*'a)      (* find (k,v) with min k in a pqueue or raise Empty *)
  val deleteMin : 'a pqueue -> (key*'a) * 'a pqueue   (* same as findMin, but also deletes (k,v) *)

end

(* Imperative Priority Queues with decreaseKey *)

signature IPQUEUE =
sig

  structure Key : ORDERED                          (* contains key type t along with an ordering *)
  type key = Key.t                         (* the type of keys used as priorities in the ipqueue *)

  type 'a ipqueue        (* imperative priority queues with the above key type and value type 'a *)
  type 'a t = 'a ipqueue
  type 'a insertedRef        (* a reference to an inserted element in a ipqueue, for decreaseKey *)

  exception Empty                                       (* raised by findMin and deleteMin below *)
  val empty     : unit -> 'a ipqueue                       (* creates a new empty priority queue *)
  val singleton : key * 'a -> 'a ipqueue     (* creates a new priority queue with one (key, val) *)

  val isEmpty   : 'a ipqueue -> bool              (* returns true if the priority queue is empty *)

  val insert    : 'a ipqueue -> (key*'a) -> unit                            (* imperative insert *)
  val insertRef : 'a ipqueue -> (key*'a) -> 'a insertedRef   (* insert and return an insertedRef *)

  val meldInto  : 'a ipqueue -> 'a ipqueue -> unit  (* moves all (k,v) in the 2nd ipq to the 1st *)

  val findMin   : 'a ipqueue -> (key*'a)   (* find (k,v) with min k in an ipqueue or raise Empty *)
  val deleteMin : 'a ipqueue -> (key*'a)          (* the same as findMin, but also deletes (k,v) *)

   (* Decrease the key in the insertedRef, which must be from a call to insertRef on the *
    * ipqueue, or one melded into it, whose element hasn't subsequently been deleted.    *)
  val decreaseKey : 'a ipqueue -> 'a insertedRef -> key -> unit

  val keys : 'a ipqueue -> key list    (* sorted keys for debugging, preserves ipqueue structure *)

end

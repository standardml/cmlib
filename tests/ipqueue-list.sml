(* A reference implementation of imperative priority heaps using unordered lists.  
   Slow, but with a very straightforward implementation.  
   Intended to be used for debugging other implementations.    *)

functor ListIPQueue (Key : ORDERED where type t=int)  :> IPQUEUE where type Key.t=Key.t =
struct
  structure Key = Key
  type key = Key.t

  type 'a ipqueue = (key ref * 'a) list ref 
  type 'a t = 'a ipqueue

  type 'a insertedRef = key ref

  fun cmp ((k1,v1), (k2,v2)) = Key.compare (!k1,!k2)
  fun export (k,v) = (!k,v)

  exception Empty

  fun empty() = ref []

  fun isEmptyNode [] = true 
    | isEmptyNode _ = false
  fun isEmpty pq = isEmptyNode (!pq)

  fun singleton(k,v) = ref [(ref k,v)]

  fun meldInto q1 q2 =   q1 := (!q1 @ !q2)   (* Returns q1 with q2 melded into it. *)

  fun insertRef q (k,v) = 
      let val kref = ref k
          val () = q := (!q) @ [(kref,v)]
      in kref      (* can be passed to decreaseKey *)
      end     

  fun insert q (k,v) = case insertRef q (k,v) of _ => ()

  fun findMin q = case Mergesort.sort cmp (!q) of [] => raise Empty
                                              | h::t => export h

  fun deleteMin q = case Mergesort.sort cmp (!q) of [] => raise Empty
                                              | h::t => ( q:=t; export h )

  fun decreaseKey ipq kref newk = 
        kref := newk            (* modify key in place *) 

  fun keys pq = 
      Mergesort.sort Key.compare (map (fn (kref, _) => !kref) (!pq))
      

end

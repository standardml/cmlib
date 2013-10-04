(* Pairing heap implementation of priority queues.  *)

(* This provides some of the best amortized performance in theory and
   practice among priority queue implementations.  It's implemented
   functionally but heavy persistent branching can cause it to repeat
   expensive operations and fail to meet its amortized bounds.  
   (See pqueue-lazy-pairing.sml)
 *)

functor PairingPQueue (Key : ORDERED) : PQUEUE =
struct
  structure Key = Key
  type key = Key.t

  datatype 'a pqueue = EmptQ
                     | Node of {value: key * 'a, children: 'a pqueue list} 
  type 'a t = 'a pqueue

  fun cmp((k1,v1),(k2,v2)) = Key.compare(k1,k2)

  exception Empty

  fun empty () = EmptQ
  fun isEmpty EmptQ = true 
    | isEmpty _ = false

  fun singleton(k,v) = Node {value=(k,v), children=[]}

  fun meld a EmptQ = a
    | meld EmptQ b = b
    | meld (n1 as Node {value=v1, children=cs1})
	    (n2 as Node {value=v2, children=cs2}) =
        case cmp(v1, v2) of 
           LESS => Node {value=v1, children=n2::cs1}
         | _    => Node {value=v2, children=n1::cs2}

  fun insert pq v = meld (singleton v) pq

  fun findMin EmptQ = NONE
    | findMin (Node {value=v, ...}) = SOME v

  fun lookupMin EmptQ = raise Empty
    | lookupMin (Node {value=v, ...}) = v

  (* This is "2-pass" linking, which is the most standard for pairing heaps. *)
  fun mergePairs [] = EmptQ
    | mergePairs [c] = c
    | mergePairs (c1::c2::cs) = meld (meld c1 c2) (mergePairs cs)

  fun deleteMin EmptQ = raise Empty
    | deleteMin (Node {value=v, children=[]}) = (v, EmptQ)
    | deleteMin (Node {value=v, children=[c]}) = (v, c)
    | deleteMin (Node {value=v, children=cs}) = (v, mergePairs cs)

end

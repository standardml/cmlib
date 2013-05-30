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

  datatype 'a pq = Empty 
                 | Node of {value: key * 'a, children: 'a pq list} 
  type 'a t = 'a pq

  fun cmp((k1,v1),(k2,v2)) = Key.compare(k1,k2)

  exception EMPTY

  fun empty () = Empty
  fun isEmpty Empty = true 
    | isEmpty _ = false

  fun singleton(k,v) = Node {value=(k,v), children=[]}

  fun meld a Empty = a
    | meld Empty b = b
    | meld (n1 as Node {value=v1, children=cs1})
	    (n2 as Node {value=v2, children=cs2}) =
        case cmp(v1, v2) of 
           LESS => Node {value=v1, children=n2::cs1}
         | _    => Node {value=v2, children=n1::cs2}

  fun insert v Q = meld (singleton v) Q

  fun findMin Empty = NONE
    | findMin (Node {value=v, ...}) = SOME(v)

  (* This is "2-pass" linking, which is the most standard for pairing heaps. *)
  fun mergePairs [] = Empty
    | mergePairs [c] = c
    | mergePairs (c1::c2::cs) = meld (meld c1 c2) (mergePairs cs)

  fun deleteMin Empty = (NONE,Empty)
    | deleteMin (Node {value=v, children=[]}) = (SOME(v), Empty)
    | deleteMin (Node {value=v, children=[c]}) = (SOME(v), c)
    | deleteMin (Node {value=v, children=cs}) = (SOME(v), mergePairs cs)

end

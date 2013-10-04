(* This priority queue implementation provides log(n) per-operation
  bounds.  If amortized bounds suffice there are other faster
  implementations like pairing heaps.  Although, with heavy use of
  persistent branching this leftist heap might prove the best option.
 *)

(* Based on LeftistHeap.sml from 15-210 at CMU *)

functor LeftistPQueue (Key : ORDERED) : PQUEUE =
struct
  structure Key = Key
  type key = Key.t

  datatype 'a pqueue = EmptyQ
                     | Node of {rank : int, value: key * 'a, 
	                        left : 'a pqueue, right : 'a pqueue} 
  type 'a t = 'a pqueue

  fun cmp((k1,v1),(k2,v2)) = Key.compare(k1,k2)

  exception Empty

  fun empty () = EmptyQ
  fun isEmpty EmptyQ = true 
    | isEmpty _ = false

  fun rnk EmptyQ = 0
    | rnk (Node {rank=r,...}) = r

  fun node (v, l, r) =
    let val rr = rnk r
        val lr = rnk l
        val nr = if (rr < lr) then rr+1 else lr+1
    in Node {rank=nr, value=v, left=l, right=r}
    end

  fun singleton(k,v) = Node {rank=1, value=(k,v), left=EmptyQ, right=EmptyQ}

  fun meld a EmptyQ = a
    | meld EmptyQ b = b
    | meld (n1 as Node {value=v1, left=l1, right=r1, ...})
	    (n2 as Node {value=v2, left=l2, right=r2, ...}) =
        case cmp(v1, v2) of 
           LESS => node (v1, l1, meld r1 n2)
         | _    => node (v2, l2, meld n1 r2)

  fun insert pq v = meld (singleton v) pq

  fun findMin EmptyQ = NONE
    | findMin (Node {value=v, ...}) = SOME v

  fun lookupMin EmptyQ = raise Empty
    | lookupMin (Node {value=v, ...}) = v

  fun deleteMin EmptyQ = raise Empty
    | deleteMin (Node {value=v, left=l, right=r, ...}) = (v, meld l r)
end

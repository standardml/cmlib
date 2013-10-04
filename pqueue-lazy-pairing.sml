(* Pairing heap implementation of priority queues that uses laziness
 * to retain amortized bounds with persistent use.  Alas, the use of
 * laziness adds significantly to the constant factors, so the original 
 * pairing heap may be better in most cases, and the leftist heap may
 * be better when there is heavy use of persistent branching.
 *)

functor LazyPairingPQueue (Key : ORDERED) : PQUEUE =
struct
  structure Key = Key
  type key = Key.t

  datatype 'a pqueue = EmptQ
                     | Node of {value: key * 'a, odd: 'a pqueue, sus: 'a pqueue Susp.susp}
  type 'a t = 'a pqueue

  fun cmp((k1,v1),(k2,v2)) = Key.compare(k1,k2)

  exception Empty

  fun empty() = EmptQ
  fun isEmpty EmptQ = true 
    | isEmpty _ = false

  fun singleton(k,v) = Node {value=(k,v), odd=EmptQ, sus=Susp.delay (fn()=> EmptQ)}

  fun meld q EmptQ = q
    | meld EmptQ q = q
    | meld (p as Node {value=v1, ... }) (q as Node {value=v2, ... }) = 
        case cmp(v1,v2) of
           LESS => link p q
         | _    => link q p

  and link (Node {value=v, odd=EmptQ, sus}) c = Node {value=v, odd=c, sus=sus}
    | link (Node {value=v, odd, sus}) c = 
         Node {value=v, odd=EmptQ, sus=Susp.delay(fn()=> meld (meld c odd) (Susp.force sus))  }
    | link _ _ = raise Match

  fun insert pq v = meld pq (singleton v)

  fun findMin EmptQ = NONE
    | findMin (Node {value=v, ...}) = SOME v

  fun lookupMin EmptQ = raise Empty
    | lookupMin (Node {value=v, ...}) = v

  fun deleteMin EmptQ = raise Empty
    | deleteMin (Node {value=v, odd, sus}) = (v, meld odd (Susp.force sus))
end


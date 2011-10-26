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

  datatype 'a pq = Empty 
                 | Node of {value: key * 'a, odd: 'a pq, sus: 'a pq Susp.susp} 
  type 'a t = 'a pq

  fun cmp((k1,v1),(k2,v2)) = Key.compare(k1,k2)

  exception EMPTY

  fun empty() = Empty
  fun isEmpty Empty = true 
    | isEmpty _ = false

  fun singleton(k,v) = Node {value=(k,v), odd=Empty, sus=Susp.delay (fn()=> Empty)}

  fun meld q Empty = q
    | meld Empty q = q
    | meld (p as Node {value=v1, ... }) (q as Node {value=v2, ... }) = 
        case cmp(v1,v2) of
           LESS => link p q
         | _    => link q p

  and link (Node {value=v, odd=Empty, sus}) c = Node {value=v, odd=c, sus=sus}
    | link (Node {value=v, odd, sus}) c = 
         Node {value=v, odd=Empty, sus=Susp.delay(fn()=> meld (meld c odd) (Susp.force sus))  }

  fun insert v Q = meld (singleton v) Q

  fun findMin Empty = NONE
    | findMin (Node {value=v, ...}) = SOME(v)

  fun deleteMin Empty = (NONE,Empty)
    | deleteMin (Node {value=v, odd, sus}) = (SOME(v), meld odd (Susp.force sus))
end


(* Author: Rowan Davies <rowan.davies@uwa.edu.au>

   This is an pairing heap implementation of imperative priority queues, supporting decreaseKey.
   All operations are constant time aside from deleteMin which is amortized O(log n) time 
   unless decreaseKey is called much more often than deleteMin.

   Hence it suitable for use in a number of algorithms that depend on an efficient decreaseKey.
   If the decreaseKey operation isn't needed one of the non-imperative implementations may be 
   faster and preferable.

   In practice this data structure tends to be faster than alternatives like Fibonacci heaps
   in basically every situation where decreaseKey is required.
 *)


functor PairingIPQueue (Key : ORDERED) 
         :> IPQUEUE where type Key.t=Key.t =
struct
    structure Key = Key
    type key = Key.t


(*  type 'a nref = int * 'a ref
    val dbgrefcount = ref 0
    fun nref x = (dbgrefcount := !dbgrefcount+1; (!dbgrefcount, ref x))
    infix 3 :==
    fun (_,r):==v  =  r:=v
    fun !! (_, r) = !r      *)

  (* nref below is the same as ref, but the above alternative has been handy for debugging. *)
  (* (This could be a functor arg, but maybe with a performance hit if it's not inlined.) *)

    type 'a nref = 'a ref
    infix 3 :==
    val nref = ref
    val op:==  =  op:=
    val !! = !


    (* Invariant: "prev" holds the parent for the firstChild, and the predecessor otherwise.  *)
    datatype 'a pqnode = EmptQ
                       | Node of {key: key ref, value: 'a, prev: 'a ipqueue, 
                                  firstChild: 'a ipqueue, succ: 'a ipqueue}
     withtype 'a ipqueue = 'a pqnode nref 

    type 'a t = 'a ipqueue

    type 'a insertedRef = 'a ipqueue

    infix 4 ==  (* Test for two references to the same non-empty node.  For speed, just compare key refs. *)  
    fun r1 == r2 =  case (!!r1, !!r2) of (Node {key=k1, ...}, Node {key=k2, ...}) => k1=k2
                                       | _ => false
    exception Empty
    fun empty() = nref EmptQ

    fun isEmptyNode EmptQ = true 
      | isEmptyNode _ = false

    fun isEmpty pq = isEmptyNode (!!pq)

    fun singleton(k,v) = nref (Node {key=ref k, value=v, prev=empty(), firstChild=empty(), succ=empty() })

    (* The following is optimized for constant factors, complicating the invariants a little. *)
    (* This is particularly true for meld0, which is optmized for the particular calls later on. *)

    fun setPrev q newp = case !!q of
                           EmptQ => ()
                         | Node{prev=pref, ...} => pref:==newp 

    fun mkSucc n succ snew = ( succ := !!snew ;  setPrev snew n )
    fun insFCh n succ fc2 = ( mkSucc n succ fc2 ; fc2 :== n )

    (* O(1): meld q1 q1 returns either q1 or q2, with the other melded into it. *)
    (* For non-empty nodes, (!q1).succ or (!q2).prev are treated as empty and are overwritten. *)
    (* (!result).prev will be the original (!q1).prev *)
    (* (!result).succ will be the original (!q2).succ *)
    (* But (!(!result).prev).succ/firstChild aren't modified - the calling code should do this. *)
    fun meld q1 q2 = case (!!q1, !!q2) of
            (_, EmptQ) => q1
          | (EmptQ, _) => q2
          | (n1 as Node {key=k1, prev=p1, firstChild=fc1, succ=s1, ...},
             n2 as Node {key=k2, prev=p2, firstChild=fc2, succ=s2, ...} ) =>
               case Key.compare(!k1, !k2) of 
                  LESS => ( mkSucc n1 s1 s2  ;  (* Put s2 as successor of n1 *)
                            p2 :== n1;          (* Make n2 have n1 as parent.  *) 
                            insFCh n2 s2 fc1 ;  (* Insert n2 at the front of fc1 *)
                            q1 )
                | _    => ( p2 :== !!p1;        (* Put n2 at the top  *)
                            p1 :== n2;          (* Make n1 have n2 as parent. *)
                            insFCh n1 s1 fc2 ;  (* Insert n1 at the front of fc2 *)
                            q2 ) 


    (* O(1): inserts kv into q.  q should be a root node.  Returns a ref for decreaseKey.  *)
    fun insertRef q kv = 
        let val q1 = singleton kv 
            val () = (q :== !!(meld q1 q))
        in q1      (* q1 can be passed to decreaseKey *)
        end     

    (* O(1): inserts kv into q.  q should be a root node.  *)
    fun insert q kv = ( insertRef q kv; () ) 

    (* O(1) *)
    fun findMin q = case !!q of EmptQ => raise Empty
                             | (Node {key=k, value=v, ...}) => (!k, v)

    (* This is "2-pass" linking, which is the most standard for pairing heaps. *)
    (* If q1 has two successors q2 and q3, detatch and meld them, recursively for q3. *)  
    fun mergePairs q1 = case !!q1 of
        EmptQ => q1
      | Node {succ=q2, ...} => case !!q2 of 
            EmptQ => q1
          | Node {succ=q3, ...} =>
                let val (q2',q3') = (nref (!!q2), nref (!!q3)) in
                    q2 :== EmptQ; q3 :== EmptQ;           (* detach succ in q1 and q2 *)
                    meld (meld q1 q2') (mergePairs q3')   (* result.prev = (!q1).prev *)
                end          (* (!q2').succ is EmptQ, hence so is (meld q1 q2').succ  *)

    (* O(log n) amortized *)
    fun deleteMin q = case !!q of 
        EmptQ => raise Empty
      | Node {key=k, value=v, firstChild=fc, ...} =>  ( q :== !!(mergePairs fc);  (!k, v) )


    (* O(1), but affects the amortized bound for deleteMin, for which it must be counted as  *)
    (* O(2^sqrt(log log n)) amortized but this grows very slowly - it is <3 for n<10^300.  *)
    fun decreaseKey root insRef newk = case !!insRef of
       EmptQ =>  raise Fail " ImpPairingPQueue: called decreaseKey on a deleted node."
     | Node {key=k1, value=v1, prev=p1, firstChild=fc1, succ=q2} => 
         (k1 := newk;            (* modify key, if necessary detach node, then meld with root *)

           if insRef == root then () else
             case !!p1 of 
                  EmptQ => raise Fail "ImpPairingPQueue: impossible - non-root node has no parent"
                | Node {key=p1k, firstChild=p1fc, succ=p1s, ...} => 
                  case Key.compare(newk, !!p1k) of 
                    LESS => ( setPrev q2 (!!p1);
                              (if p1fc == insRef            (* If insRef is a first child *)
                                  then p1fc :== (!!q2)      (* update the parent  *)
                                  else p1s :== (!!q2)  ) ;  (* else update the predecessor. *)
                              q2:==EmptQ;
                              root :== !!(meld root insRef)   (* Always overwrites insRef.prev *) 
                            ) 
                  | _ => ()  (* No need to restructure if the new key isn't less than the parent. *)
         )


    (* keys is mostly for debugging, hence this code doesn't modify the pqueue structure. *)
    fun keys0 q = case !!q of 
        EmptQ => []
      | Node {key=k, value=v, prev, firstChild, succ} => (!k) :: keys0 firstChild @ keys0 succ

    fun keys pq = Mergesort.sort Key.compare (keys0 pq)

    fun meldInto q1 q2 = 
        let val qnew = meld q1 q2 in
          ( if q1 = qnew then () else
               q1 := !!qnew ) ;     
          q2 := EmptQ
        end

end

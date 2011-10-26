(* Imperative pairing heap implementation of priority queues, supporting decreaseKey hence
   making it suitable for use in a number of important algorithms.  If this operation isn't
   needed one of the non-imperative implementations may be faster and preferable.

   In practice this data structure tends to be faster than alternatives like Fibonacci heaps
   in basically every situation where decreaseKey is required.  *)

(* NOT PROPERLY TESTED AND LIKELY TO HAVE BUGS *)

functor ImperativePairingPQueue (Key : ORDERED) : IMPERATIVE_PQUEUE =
struct
  structure Key = Key
  type key = Key.t

  datatype 'a pqnode = Empty 
                     | Node of {value: key ref * 'a, parent: 'a pq, firstChild: 'a pq, succ: 'a pq}
   withtype 'a pq = 'a pqnode ref 

  type 'a t = 'a pq

  type 'a pqNodeRef = 'a pq

  fun cmp((k1,v1),(k2,v2)) = Key.compare(!k1,!k2)

  exception EMPTY

  fun empty() = ref Empty

  fun isEmptyNode Empty = true 
    | isEmptyNode _ = false
  fun isEmpty pq = isEmptyNode (!pq)

  fun singleton(k,v) = ref (Node {value=(ref k,v), parent=empty(), firstChild=empty(), succ=empty() })

  (* Returns one of the two original refs, with the other melded into it. *)
  fun meld q1 q2 = 
      (case (!q1, !q2) of
          (_, Empty) => q1
        | (Empty, _) => q2
        | (n1 as Node {value=v1, parent=p1, firstChild=fc1, succ=s1},
	   n2 as Node {value=v2, parent=p2, firstChild=fc2, succ=s2}) =>
             case cmp(v1, v2) of 
                LESS => (p2:=n1; s2:= !fc1; fc1:=n2; q1)
              | _    => (p1:=n2; s1:= !fc2; fc2:=n1; q2) 
      )

  fun insert v q = 
      let val q1 = singleton v 
          val () = (q := !(meld q1 q))
      in q1      (* q1 can be passed to decreaseKey *)
      end     

  fun findMin q = case !q of Empty => NONE
                           | (Node {value=v, ...}) => SOME(v)

  (* This is "2-pass" linking, which is the most standard for pairing heaps. *)
  fun mergePairs (q1 as ref Empty) = q1
    | mergePairs (q1 as ref (Node {succ=ref Empty, ...})) = q1
    | mergePairs (q1 as ref (Node {succ=q2 as ref (Node {succ=q3, ...}),...})) =
                 meld (meld q1 q2) (mergePairs q3)

  fun deleteMin (q as ref Empty) = NONE
    | deleteMin (q as ref (Node {value=v, firstChild=qc as ref Empty, ...})) = (q := !qc; SOME(v) )
    | deleteMin (q as ref (Node {value=v, firstChild=fc1,...})) = (q := !(mergePairs fc1); SOME(v) )

  fun decreaseKey (q as ref (Node {value=(v1,k1), parent=p1, firstChild=fc1, succ=q2})) root newv = 
     (v1 := newv;            (* modify key, detach node, then meld with root *)
 
      if q <> root then 
         (case !q2 of Empty => ()
                    | Node {parent=p2,...} => p2 := !p1;
          (case !p1 of Node {firstChild=p1fc, succ=p1s, ...} => 
              if p1fc=q then p1fc := (!q2)
              else p1s := (!q2)
            | _ => raise Fail "ImpPairingPQueue: impossible - non-root node has no parent" );
          q2:=Empty;

          root := !(meld root q)
         )
      else () 
     )

    | decreaseKey _ _ _ =  raise Fail " ImpPairingPQueue: called decreaseKey on a deleted node."

end

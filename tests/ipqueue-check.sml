(* A testing implementation of imperative priority heaps that performs
   all operations on both the fast pairing implementation and the slow
   list implementation.  Any deviations between the two are reported.  *)

functor CheckIPQueue (Key : ORDERED where type t = int)  
        :> IPQUEUE where type Key.t=Key.t =
struct
  structure Key = Key : ORDERED where type t=int
  structure P = PairingIPQueue(Key) (* :> IPQUEUE where type key=int *)
  structure L = ListIPQueue(Key)    (* :> IPQUEUE where type key=int *)
  type key = Key.t 

  type 'a ipqueue = 'a P.ipqueue * 'a L.ipqueue 
  type 'a t = 'a ipqueue

  type 'a insertedRef = 'a P.insertedRef * 'a L.insertedRef

  exception Empty

  fun dbg _ = "_"

  fun empty() = (P.empty(), L.empty())

  fun isEmpty (p,l) = 
      case (P.isEmpty p, L.isEmpty l)
        of (true, true) => true
         | (false, false) => false 
         | _ => raise Fail "ImperativeCheckQueue.isEmptyNode"

  fun singleton(k,v) = (P.singleton(k,v), L.singleton(k,v))

  (* Returns one of the two original refs, with the other melded into it. *)
  (* Well, for this implementation each of the two component structures do this.  *)
  fun meldInto (p1,l1) (p2, l2) = ( P.meldInto p1 p2 ;  L.meldInto l1 l2 ; () )

  fun insertRef (p, l) kv = (P.insertRef p kv, L.insertRef l kv)
  fun insert (p, l) kv = ( P.insert p kv ;  L.insert l kv; () )

  fun findMin (p, l) = 
      case (SOME (P.findMin p) handle P.Empty => NONE, 
            SOME (L.findMin l) handle L.Empty => NONE)
        of (NONE, NONE) => raise Empty
         | (SOME(pk,pv), SOME(lk,lv)) => 
              (case Key.compare (pk,lk)
                 of EQUAL =>  raise Fail "ImperativeCheckQueue.findMin: keys differ"
                  | _ => (pk,pv) )
         | _ => raise Fail("ImperativeCheckQueue.deleteMin: foundness differs ")


  fun deleteMin (p,l) = 
      case (SOME (P.deleteMin p) handle P.Empty => NONE, 
            SOME (L.deleteMin l) handle L.Empty => NONE)
        of (NONE, NONE) => raise Empty
         | (SOME(pk,pv), SOME(lk,lv)) => 
              (case Key.compare (pk,lk)
                 of EQUAL => (pk,pv)
                  | _ =>   raise Fail ("ImperativeCheckQueue.deleteMin: keys differ " 
                                          ^ Int.toString pk ^ " <> " ^ Int.toString lk)  )
         | _ => raise Fail("ImperativeCheckQueue.deleteMin: foundness differs ")


  fun decreaseKey (proot, lroot) (pInsRef,lInsRef) newk = 
      ( P.decreaseKey proot pInsRef newk ; 
        L.decreaseKey lroot lInsRef newk )

fun keysToString [] = ""
    | keysToString (h::t) =  (Int.toString h) ^ ", " ^ (keysToString t)

  fun keys (p,l) =
      let val (pkeys, lkeys) = (P.keys p, L.keys l)
      in 
        if pkeys=lkeys then pkeys
        else raise Fail ("ImperativeCheckQueue.keys: keys differ " 
                                          ^ keysToString pkeys ^ " <> " ^ keysToString lkeys)
      end     
end

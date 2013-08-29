
structure ImpPQ = CheckIPQueue(
struct
      type t = int
      val eq : t*t -> bool = (op =)      
      val compare = Int.compare
 end)

functor PQImpTest( structure Rand : RAND )
 = struct

local open ImpPQ in 

  fun keysToString [] = ""
    | keysToString (h::t) =  (Int.toString h) ^ ", " ^ (keysToString t)

  fun dbg pq = "" (* keysToString (keys pq) *)
  fun dbgPrint s = () (* print s  *)
  fun pr str (i,s) = dbgPrint (str ^ " = " ^ (Int.toString i) ^ ", " ^ s ^ "\n" )


  val pq1 : string ipqueue = empty () 
  val _ = insert pq1 (10, "a")
  val _ = insert pq1 (5, "b")

  fun try f = SOME (f ()) handle Empty => NONE
  fun tryDeleteMin pq = try (fn () => deleteMin pq)

  val (SOME x1) = tryDeleteMin pq1
  
  val r = insertRef pq1 (15, "c")
  val rd = insertRef pq1 (12, "d")

  val () = dbgPrint (dbg pq1) 
   
   val _ = decreaseKey pq1 r 2
(*  val _ = decreaseKey pq1 rd 3 *)

  val (SOME x2) = SOME (deleteMin pq1) handle Empty => NONE
  val (SOME x3) = SOME (deleteMin pq1) handle Empty => NONE
  val (SOME x4) = SOME (deleteMin pq1) handle Empty => NONE
  val NONE      = SOME (deleteMin pq1) handle Empty => NONE

  val _ = pr "x1" x1
  val _ = pr "x2" x2
  val _ = pr "x3" x3
  val _ = pr "x4" x4

  val pq : string ipqueue = empty () 
  val tests =  [50, 75, 22, 33, 71, 50, 10, 5, 4, 3, 2, 1]
  val nodes = List.map (fn x => (insertRef pq (x,Int.toString x); dbgPrint (dbg pq ^ "\n") )) tests
  val res = List.map (fn _ => (dbgPrint (dbg pq ^ "\n"); try (fn () => deleteMin pq))) [1,2,3]

  fun pr str (SOME(i,_)) = dbgPrint (str ^ " = " ^ (Int.toString i)  ^ "\n" )
    | pr str NONE = dbgPrint "NONE\n"

  val () = List.app (pr "") res

  val tests =  [50, 75, 22, 33, 71, 50, 10, 5, 4, 3, 2, 1]
  val nodes = List.map (fn x => (insertRef pq (x,Int.toString x); dbgPrint (dbg pq ^ "\n") )) tests
  val res = List.map (fn _ => (dbgPrint (dbg pq ^ "\n"); try (fn () => deleteMin pq))) tests

  fun pr str (SOME(i,_)) = dbgPrint (str ^ " = " ^ (Int.toString i)  ^ "\n" )
    | pr str NONE = dbgPrint "NONE\n"

  val () = List.app (pr "") res


  (* Randomized testing *)
 
  val pqr : (bool ref) ipqueue = singleton (~99, ref true) (* empty() *)

  val inserted:(int ref * bool ref * (bool ref) insertedRef) list ref = ref []
  fun addInserted k r nodeRef = inserted := (ref k,r,nodeRef) :: (!inserted)

  fun delInserted k = inserted := List.filter (fn (ref kk, _, _) => Key.compare(k, kk) <> EQUAL  ) (!inserted)


  fun pickInserted () = 
      let val active = List.filter (fn (k,r,nodeRef) => !r) (!inserted)
      in  (inserted:=active ;
          if List.length active = 0 then NONE
          else SOME (List.nth (!inserted, Rand.randInt (List.length (!inserted))))
          )
      end

  fun keysToString [] = ""
    | keysToString (h::t) =  (Int.toString h) ^ ", " ^ (keysToString t)

  (* So far this only tests sequences of insert, deleteMin and decreaseKey *)
  fun pqrTests 0 _ = ()
    | pqrTests n pqrCurr = 
        (case Rand.randInt 3
           of 0 =>   let val k = Rand.randInt 50
                         val r = ref true
                         val nodeRef : (bool ref) insertedRef = insertRef pqrCurr (k, r)
                     in ( addInserted k r nodeRef ; 
                          pqrTests (n-1) pqrCurr )
                     end
            | 1 => (case tryDeleteMin pqrCurr
                        of NONE => (dbgPrint "Empty\n" ; pqrTests (n-1) pqrCurr)
                         | SOME (mink, minv) => ( dbgPrint (Int.toString mink ^ "\n") ; 
                                                  (* minv := false; *)
                                                  delInserted mink ; 
                                                  pqrTests (n-1) pqrCurr ) )
            | 2 => (case pickInserted() 
                        of NONE => pqrTests n pqrCurr  (* Do another test instead *)
                         | SOME (kref, r, nodeRef) => 
                           let val k = !kref
                               val newk = k - 1 - Rand.randInt 20
                           in ( dbgPrint ("decreaseKey " ^ Int.toString k ^ " to " ^ Int.toString newk ^ "\n") ;
                                dbgPrint ("keys: " ^ keysToString (keys pqrCurr) ^ "\n") ;
                                decreaseKey pqr nodeRef newk ;
                                kref:=newk ;
                                dbgPrint ("(end)keys: " ^ keysToString (keys pqrCurr) ^ "\n") ;
                                pqrTests (n-1) pqrCurr )
                           end)
            | _ => raise Fail "Rand.randInt returned an unexpected number."
       )

   val () = pqrTests 10000 pqr
   val () = print "PairingIPQueue passed all tests\n"

end
end

structure PQImpTestResult = PQImpTest( structure Rand = MTRand )
                          

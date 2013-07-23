structure QuicksortTest :> sig end =
struct
val () = print "Quicksort test (quicksort-test.sml)\n"
val () = Testing.reset ()
		       
fun iter i j = if i > j then [] else i :: iter (i + 1) j
fun downiter i j = if i < j then [] else i :: downiter (i - 1) j
						       
val () = List.app
             (fn j =>
                 let val to_j = iter 1 j
                 in
                     Testing.expectEq (Quicksort.sort Int.compare to_j) to_j
				      ("Quicksort should sort [1.." ^ Int.toString j ^ "]")
                 end)
             (iter 0 10)
	     
val () = List.app
             (fn j =>
                 let val to_j = iter j 10
                     val dto_j = downiter 10 j
                 in
                     Testing.expectEq (Quicksort.sort Int.compare dto_j) to_j
				      ("Quicksort should sort [10 downto " ^ Int.toString j
				       ^ "]")
                 end)
             (downiter 11 1)
	     
fun intListToString l =
    "[" ^ String.concatWith ", " (List.map Int.toString l) ^ "]"
								 
val () = Testing.report ()
end

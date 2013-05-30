(* MergesortTest is only evaluated for its effects *)
structure MergesortTest :> sig end =
  struct
    val () = print "Mergesort test (mergesort-test.sml)\n"

    val () = Testing.reset ()

    fun iter i j = if i > j then [] else i :: iter (i + 1) j
    fun downiter i j = if i < j then [] else i :: downiter (i - 1) j

    val () = List.app
                (fn j =>
                    let val to_j = iter 1 j
                    in
                        Testing.expectEq (Mergesort.sort Int.compare to_j) to_j
                          ("Mergesort should sort [1.." ^ Int.toString j ^ "]")
                    end)
                (iter 0 10)

    val () = List.app
                (fn j =>
                    let val to_j = iter j 10
                        val dto_j = downiter 10 j
                    in
                        Testing.expectEq (Mergesort.sort Int.compare dto_j) to_j
                          ("Mergesort should sort [10 downto " ^ Int.toString j
                          ^ "]")
                    end)
                (downiter 11 1)

    fun intListToString l =
        "[" ^ String.concatWith ", " (List.map Int.toString l) ^ "]"

    val () = List.app
                (fn l =>
                    Testing.expectEq (Mergesort.sort (fn _ => EQUAL) l) l
                      ("Mergesort should be stable " ^ intListToString l))
            [ [],
              [1],
              [1,2], [2,1],
              [1,2,3], [1,3,2],
              [3,1,2], [3,2,1],
              [2,1,3], [2,3,1],
              [1,2,3,4], [1,2,4,3], [1,3,2,4], [1,3,4,2], [1,4,2,3], [1,4,3,2],
              [2,1,3,4], [2,1,4,3], [2,3,1,4], [2,3,4,1], [2,4,1,3], [2,4,3,1],
              [3,1,2,4], [3,1,4,2], [3,2,1,4], [3,2,4,1], [3,4,1,2], [3,4,2,1],
              [4,1,2,3], [4,1,3,2], [4,2,1,3], [4,2,3,1], [4,3,1,2], [4,3,2,1] ]

    val () = Testing.report ()
  end

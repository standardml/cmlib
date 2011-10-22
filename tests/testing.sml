structure Testing :> TESTING =
  struct
    val n_passes = ref 0
    val n_failures = ref 0

    fun passes () = !n_passes
    fun failures () = !n_failures

    fun reset () = (n_passes := 0; n_failures := 0)

    fun report () =
        print (Int.toString (passes ()) ^ " tests passed, "
             ^ Int.toString (failures ()) ^ " tests failed.\n")

    fun incr r = r := !r + 1

    fun expect x p s =
        if p x
        then (incr n_passes)
        else (print ("Failed test: " ^ s ^ "\n");
              incr n_failures)

    fun expectEq x y s = expect x (fn x => x = y) s

  end

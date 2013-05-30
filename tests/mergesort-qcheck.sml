(* test mergesort using quickcheck *)
structure MergesortQCheck =
  struct
    (* some utility stuff and fixity declarations *)
    open QCheck
    infixr 3 ==>
    infix 5 /\
    fun (p /\ q) x = p x andalso q x

    fun leq compare (x, y) =
        case compare (x, y) of
            LESS => true
          | EQUAL => true
          | GREATER => false

    fun lt compare (x, y) = compare (x, y) = LESS
    fun eq compare (x, y) = compare (x, y) = EQUAL

    (*** specification function: sorted ***)

    fun is_sorted p nil = true
      | is_sorted p [_] = true
      | is_sorted p (x::y::xs) = leq p (x, y) andalso is_sorted p (y::xs)


    (*** specification function: stably sorted ***)

    (* [is_stably_sorted p orig l] iff l is sorted p-least to p-greatest and
       any run of p-equal elements in l has the same order it had in orig.*)
    (* NB: this turned out to be way more complicated than test code should
       be.. probably better to just compare to a super-simple stable sort,
       like insertion sort (see below). *)
    fun is_stably_sorted p orig nil = true
      | is_stably_sorted p orig [_] = true
      | is_stably_sorted p orig (x::y::xs) =
            (lt p (x, y) andalso is_stably_sorted p orig (y::xs))
            orelse
            let val orig_eq_to_x = List.filter (fn y => eq p (x, y)) orig
            in
                (eq p (x, y) andalso stable_run p orig orig_eq_to_x (x::y::xs))
            end

    (* [stable_run p orig xs (xs @ ys)] iff [is_stably_sorted p orig ys] *)
    and stable_run p orig nil l = is_stably_sorted p orig l
      | stable_run p orig (_::_) nil = false (* ran out of prefix! *)
      | stable_run p orig [x] (x'::ys) = x = x' andalso
                                         is_stably_sorted p orig (x'::ys)
      | stable_run p orig (x::xs) (x'::xs_ys) = x = x' andalso
                                                stable_run p orig xs xs_ys


    (*** specification: permutation ***)

    fun remove_first eq x [] = NONE
      | remove_first eq x (y::ys) =
            if eq (x, y)
            then SOME ys
            else Option.map (fn l => y::l) (remove_first eq x ys)

    fun is_permutation eq nil nil = true
      | is_permutation eq (_::_) nil = false
      | is_permutation eq nil (_::_) = false
      | is_permutation eq (x::xs) ys =
            (case remove_first eq x ys of
                NONE => false
              | SOME ys => is_permutation eq xs ys)


    (*** some printing code ***)

    fun showList show l = "[" ^ String.concatWith ", " (map show l) ^ "]"
    fun showPair (show1, show2) (x, y) = "(" ^ show1 x ^ ", " ^ show2 y ^ ")"


    (*** generators and tests below here ***)

    val () = print "Mergesort quickcheck (mergesort-qcheck.sml)\n"

    (* or:
    val genIntList = Gen.list Gen.flip Gen.Int.int
    *)
    val genIntList = Gen.vector List.tabulate (Gen.range (0, 20), Gen.Int.int)
    fun mergesortSorts p = (is_sorted p) o (Mergesort.sort p)
    fun mergesortPermutes p l = is_permutation (op=) l (Mergesort.sort p l)
    fun mergesortCorrect p = mergesortSorts p /\ mergesortPermutes p

    val () = checkGen (genIntList, SOME (showList Int.toString))
                      ("mergesort sorts", pred (mergesortCorrect Int.compare))

    (* stable mergesort should leave a list unchanged if all elts are "equal" *)
    fun mergesortStable l = l = (Mergesort.sort (fn _ => EQUAL) l)

    val () = checkGen (genIntList, SOME (showList Int.toString))
                      ("mergesort stable", pred mergesortStable)

    fun mergesortSortsStably p l = (is_stably_sorted p l) (Mergesort.sort p l)

    (* generate 100-element (char * int) pair lists, where the chars come from
       {'a', 'b', 'c'}, so it is very likely that many pairs will share a char.
       Later, we'll sort using a comparison that only looks at the char. *)
    val genPairList = Gen.vector List.tabulate
                        (Gen.range (0, 100),
                         Gen.zip (Gen.charRange (#"a", #"c"), Gen.Int.int))

    fun cmpPair ((c1, _), (c2, _)) = Char.compare (c1, c2)
    val () = checkGen (genPairList, SOME (showList (showPair (Char.toString,
                                                              Int.toString))))
                      ("mergesort stable, pairs",
                       pred (mergesortSortsStably cmpPair))


    (*** mergesort matches insertion sort, the canonical simple stable sort ***)

    fun insert p x [] = [x]
      | insert p x (y::ys) = if leq p (x, y)
                             then x::y::ys
                             else y::insert p x ys

    fun insertionSort p nil = nil
      | insertionSort p [x] = [x]
      | insertionSort p (x::xs) = insert p x (insertionSort p xs)

    fun mergesortMatchesInsertionSort p l =
        Mergesort.sort p l = insertionSort p l

    val () = checkGen (genPairList, SOME (showList (showPair (Char.toString,
                                                              Int.toString))))
                      ("mergesort = ins sort",
                       pred (mergesortMatchesInsertionSort cmpPair))
  end

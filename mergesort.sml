
structure Mergesort :> SORT =
   struct

      fun split l b1 b2 =
          (case l of
              nil =>
                 (b1, b2)
            | x :: rest =>
                 split rest b2 (x::b1))

      fun merge f l1 l2 =
          (case (l1, l2) of
              ([], _) =>
                 l2
            | (_, []) =>
                 l1
            | (x1 :: rest1, x2 :: rest2) =>
                 (case f (x1, x2) of
                     LESS =>
                        x1 :: merge f rest1 l2
                   | GREATER =>
                        x2 :: merge f l1 rest2
                   | EQUAL =>
                        x1 :: x2 :: merge f rest1 rest2))

      fun sort f l =
          (case l of
              [] =>
                 []
            | [x] =>
                 [x]
            | _ =>
                 let
                    val (b1, b2) = split l [] []
                    val b1' = sort f b1
                    val b2' = sort f b2
                 in
                    merge f b1' b2'
                 end)

   end
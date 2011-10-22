
structure Mergesort :> SORT =
   struct

      (* stable merge using the tortoise and hare algorithm: the tortoise
         moves by ones, the hare by twos, and when the hare gets to the end,
         the tortoise is halfway.  "collect" collects the front half of the
         list along the way. *)
      fun split l =
          (* invariant: hare is always a final subsequence of tortoise *)
          let fun chase collect tortoise hare =
              case (tortoise, hare) of
                  (* hare can take two steps *)
                  (x::xs, y1::y2::ys) => chase (x::collect) xs ys
                  (* hare cannot take two steps *)
                | _ => (List.rev collect, tortoise)
          in
              chase [] l l
          end

      (*
      fun split l b1 b2 =
          (case l of
              nil =>
                 (b1, b2)
            | x :: rest =>
                 split rest b2 (x::b1))
      *)

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
                   | EQUAL =>
                        x1 :: merge f rest1 l2
                   | GREATER =>
                        x2 :: merge f l1 rest2))

      fun sort f l =
          (case l of
              [] =>
                 []
            | [x] =>
                 [x]
            | _ =>
                 let
                    val (b1, b2) = split l
                    val b1' = sort f b1
                    val b2' = sort f b2
                 in
                    merge f b1' b2'
                 end)

   end

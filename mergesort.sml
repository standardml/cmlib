
structure Mergesort :> SORT =
   struct

      (* split n acc tortoise hare

         If    l = l1 @ tortoise
               l = l2 @ hare
               |l1| = n
               |l2| = 2n
               acc = rev l1
         then  l = left @ right
               |left| = |right|  or  |left| = |right|+1
               and
               return (|left|, left, |right|, right)
      *)
      fun split n acc tortoise hare =
         (case (tortoise, hare) of
             (x :: tortoise', _ :: _ :: hare') =>
                split (n+1) (x :: acc) tortoise' hare'
           | (_, [_] ) =>
                (n, rev acc, n+1, tortoise)
           | (_, []) =>
                (n, rev acc, n, tortoise)
           | _ =>
                raise (Fail "precondition"))


      (* If we know the size to split at, we can do it cheaper. *)
      fun nsplit n collect l =
         if n = 0 then
            (rev collect, l)
         else
            (case l of
                [] =>
                   raise (Fail "invariant")
              | h :: t =>
                   nsplit (n-1) (h :: collect) t)


      fun revOnto l acc =
         (case l of
             [] => acc
           | h :: t =>
                revOnto t (h :: acc))

      (* takes increasing inputs, returns decreasing *)
      fun mergeFwd f l1 l2 acc =
         (case (l1, l2) of
             ([], _) =>
                revOnto l2 acc
           | (_, []) =>
                revOnto l1 acc
           | (x1 :: rest1, x2 :: rest2) =>
                (case f (x1, x2) of
                    GREATER =>
                       mergeFwd f l1 rest2 (x2 :: acc)
                  | _ =>
                       mergeFwd f rest1 l2 (x1 :: acc)))


      (* takes decreasing inputs, returns increasing *)
      fun mergeBwd f l1 l2 acc =
         (case (l1, l2) of
             ([], _) =>
                revOnto l2 acc
           | (_, []) =>
                revOnto l1 acc
           | (x1 :: rest1, x2 :: rest2) =>
                (case f (x1, x2) of
                    GREATER =>
                       mergeBwd f rest1 l2 (x1 :: acc)
                  | _ =>
                       mergeBwd f l1 rest2 (x2 :: acc)))


      (* n=|l|, returns increasing *)
      fun sortFwd f n l =
         (case n of
             0 => []
           | 1 => l
           | 2 =>
                (case l of
                    [x, y] =>
                       (case f (x, y) of
                           GREATER =>
                              [y, x]
                          | _ =>
                              l)
                  | _ =>
                       raise (Fail "impossible"))
           | _ =>
                let
                   val i = n div 2
                   val j = n - i
                   val (l1, l2) = nsplit i [] l
                in
                   mergeBwd f (sortBwd f i l1) (sortBwd f j l2) []
                end)

      (* n=|l|, returns decreasing *)
      and sortBwd f n l =
         (case n of
             0 => []
           | 1 => l
           | 2 =>
                (case l of
                    [x, y] =>
                       (case f (x, y) of
                           GREATER =>
                              l
                         | _ =>
                              [y, x])
                  | _ =>
                       raise (Fail "impossible"))
           | _ =>
                let
                   val i = n div 2
                   val j = n - i
                   val (l1, l2) = nsplit i [] l
                in
                   mergeFwd f (sortFwd f i l1) (sortFwd f j l2) []
                end)


      fun sort f l =
         let
            val (i, l1, j, l2) = split 0 [] l l
         in
            mergeBwd f (sortBwd f i l1) (sortBwd f j l2) []
         end
         
   end

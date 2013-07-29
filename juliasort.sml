
(* Juliasort is stable, natural sort.  Natural means it expoits any
   order in the input list.  On sorted input it runs in linear time.
   
   Based on "Adaptive folds and merge sorts", by Olin Shivers.  With
   various optimizations, many of them suggested by Shivers.
*)


structure Juliasort :> SORT =
   struct

      val infinity = valOf Int.maxInt

      (* In all that follows, the ordering on equals is taken to be the
         order of appearance in the input.
      *)

      (* getrunUp f acc accsz x l
       
         If    acc is non-strictly decreasing
               accsz = |acc|
               x >= all elements of acc
         then  l = u1 @ u2
               l' contains the same elements as u1
               l' is non-strictly decreasing
               and
               return (false, l', |l'|, u2)
      *)
      fun getrunUp f acc accsz x l =
         (case l of
             [] =>
                (false, x :: acc, accsz+1, l)
           | y :: rest =>
                (case f (x, y) of
                    GREATER =>
                       (false, x :: acc, accsz+1, l)
                  | _ =>
                       getrunUp f (x :: acc) (accsz+1) y rest))
         
      (* getrunDown f acc accsz x l
       
         If    acc is strictly increasing
               accsz = |acc|
               x < all elements of acc
         then  l = u1 @ u2
               l' contains the same elements as u1
               l' is strictly increasing
               and
               return (true, l', |l'|, u2)
      *)
      fun getrunDown f acc accsz x l =
         (case l of
             [] =>
                (true, x :: acc, accsz+1, [])
           | y :: rest =>
                (case f (x, y) of
                    GREATER =>
                       getrunDown f (x :: acc) (accsz+1) y rest
                  | _ =>
                       (true, x :: acc, accsz+1, l)))


      (* getrun f x l
       
         l = u1 @ u2
         l' contains the same elements as u1
         if b then l' is sorted else l' is anti-sorted
         and
         return (b, l', |l'|, u2)

         This is a little complicated.  In order to ensure that runs are at least 3 long
         except at the input's end (not necessary, but better for performance) we hardcode
         a decision tree.
      *)
      fun getrun f x l =
         (case l of
             [] =>
                (true, [x], 1, [])
           | y :: rest =>
                (case f (x, y) of
                    GREATER =>
                       (case rest of
                           [] =>
                              (* y < x *)
                              (true, [y, x], 2, [])
                         | z :: rest' =>
                              (case f (y, z) of
                                  GREATER =>
                                     (* z < y < x *)
                                     getrunDown f [y, x] 2 z rest'
                                | _ =>
                                     (case f (x, z) of
                                         GREATER =>
                                            (* y <= z < x *)
                                            (true, [y, z, x], 3, rest')
                                       | _ =>
                                            (* y < x <= z *)
                                            (true, [y, x, z], 3, rest'))))
                  | _ =>
                       (case rest of
                           [] =>
                              (* x <= y *)
                              (true, [x, y], 2, [])
                         | z :: rest' =>
                              (case f (y, z) of
                                  GREATER =>
                                     (case f (x, z) of
                                         GREATER =>
                                            (* z < x <= y *)
                                            (true, [z, x, y], 3, rest')
                                       | _ =>
                                            (* x <= z < y *)
                                            (true, [x, z, y], 3, rest'))
                                | _ =>
                                     (* x <= y <= z *)
                                     getrunUp f [y, x] 2 z rest'))))


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


       (* roundpow p n
          If    p is a power of 2
                p <= n
          then  p' is the greatest power of two <= n
                and
                return p'
       *)
       fun roundpow p n =
          let
             val p2 = p + p
          in
             if p2 > n then
                p
             else
                roundpow p2 n
          end


       (* growFwd f s ssz p u req
          
          If    s is sorted
                ssz = |s|
                p is a power of 2
                p <= ssz  or  u=[]
                req > 0
          then  u = u1 @ u2
                l contains the same elements as s @ u1
                if b then l is sorted else l is anti-sorted
                |l| >= min(req, |s|+|u|)
                (so |l| >= req  or  u2=[])
                and
                return (b, l, |l|, u2)
       *)
       fun growFwd f s ssz p u req =
          if req <= ssz then
             (true, s, ssz, u)
          else
             (case u of
                 [] =>
                    (true, s, ssz, u)
               | h :: rest =>
                    let
                       val (rfwd, r, rsz, x) = getrun f h rest
       
                       val p' = roundpow p ssz
                       (* p' is the greatest power of 2 <= ssz *)
       
                       val (tfwd, t, tsz, y) =
                          if rfwd then
                             growFwd f r rsz 1 x p'
                          else
                             growBwd f r rsz 1 x p'

                       (* tsz >= p' unless u=[], so 2 * p' <= ssz+tsz unless u=[] *)
                    in
                       if tfwd then
                          growBwd f (mergeFwd f s t []) (ssz + tsz) (p' + p') y req
                       else
                          if ssz >= tsz then
                             growBwd f (mergeFwd f s (rev t) []) (ssz + tsz) (p' + p') y req
                          else
                             growFwd f (mergeBwd f (rev s) t []) (ssz + tsz) (p' + p') y req
                    end)

       (* as growFwd except s is anti-sorted *)
       and growBwd f s ssz p u req =
          if req <= ssz then
             (false, s, ssz, u)
          else
             (case u of
                 [] =>
                    (false, s, ssz, u)
               | h :: rest =>
                    let
                       val (rfwd, r, rsz, x) = getrun f h rest
       
                       val p' = roundpow p ssz
                       (* p' is the greatest power of 2 <= ssz *)
       
                       val (tfwd, t, tsz, y) =
                          if rfwd then
                             growFwd f r rsz 1 x p'
                          else
                             growBwd f r rsz 1 x p'

                       (* tsz >= p' unless u=[], so 2 * p' <= ssz+tsz unless u=[] *)
                    in
                       if tfwd then
                          if ssz >= tsz then
                             growFwd f (mergeBwd f s (rev t) []) (ssz + tsz) (p' + p') y req
                          else
                             growBwd f (mergeFwd f (rev s) t []) (ssz + tsz) (p' + p') y req
                       else
                          growFwd f (mergeBwd f s t []) (ssz + tsz) (p' + p') y req
                    end)


       fun sort f l =
          (case l of
              [] => []
            | h :: rest =>
                 let
                    val (rfwd, r, rsz, x) = getrun f h rest

                    val (b, l', _, _) =
                       if rfwd then
                          growFwd f r rsz 1 x infinity
                       else
                          growBwd f r rsz 1 x infinity
                 in
                    if b then l' else rev l'
                 end)

   end

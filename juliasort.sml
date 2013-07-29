
(* Juliasort is stable, natural sort.  Natural means it expoits any
   order in the input list.  On sorted input it runs in linear time.
   
   Based on "Adaptive folds and merge sorts", by Olin Shivers.  With
   various optimizations, many of them suggested by Shivers.
*)


structure Juliasort :> SORT =
   struct

      val infinity = valOf Int.maxInt

      (* acc is non-strictly decreasing (with equals in reverse order from input)
         accsz = |acc|
         x >= all elements of acc
      *)
      fun getrunUp f acc accsz x l =
         (case l of
             [] =>
                (rev (x :: acc), accsz+1, l)
           | y :: rest =>
                (case f (x, y) of
                    GREATER =>
                       (rev (x :: acc), accsz+1, l)
                  | _ =>
                       getrunUp f (x :: acc) (accsz+1) y rest))
         
      (* acc is strictly increasing
         accsz = |acc|
         x < all elements of acc
      *)
      fun getrunDown f acc accsz x l =
         (case l of
             [] =>
                (x :: acc, accsz+1, [])
           | y :: rest =>
                (case f (x, y) of
                    GREATER =>
                       getrunDown f (x :: acc) (accsz+1) y rest
                  | _ =>
                       (x :: acc, accsz+1, l)))


(****
      fun getrun f x l =
         (case l of
             [] =>
                ([x], 1, [])
           | y :: rest =>
                (case f (x, y) of
                    GREATER =>
                       getrunDown f [x] 1 y rest
                  | _ =>
                       getrunUp f [x] 1 y rest))
****)

      (* This is a little complicated.  In order to ensure that runs are at least 3 long
         except at the input's end (not necessary, but better for performance) we hardcode
         a decision tree.
      *)
      fun getrun f x l =
         (case l of
             [] =>
                ([x], 1, [])
           | y :: rest =>
                (case f (x, y) of
                    GREATER =>
                       (case rest of
                           [] =>
                              (* y < x *)
                              ([y, x], 2, [])
                         | z :: rest' =>
                              (case f (y, z) of
                                  GREATER =>
                                     (* z < y < x *)
                                     getrunDown f [y, x] 2 z rest'
                                | _ =>
                                     (case f (x, z) of
                                         GREATER =>
                                            (* y <= z < x *)
                                            ([y, z, x], 3, rest')
                                       | _ =>
                                            (* y < x <= z *)
                                            ([y, x, z], 3, rest'))))
                  | _ =>
                       (case rest of
                           [] =>
                              (* x <= y *)
                              ([x, y], 2, [])
                         | z :: rest' =>
                              (case f (y, z) of
                                  GREATER =>
                                     (case f (x, z) of
                                         GREATER =>
                                            (* z < x <= y *)
                                            ([z, x, y], 3, rest')
                                       | _ =>
                                            (* x <= z < y *)
                                            ([x, z, y], 3, rest'))
                                | _ =>
                                     (* x <= y <= z *)
                                     getrunUp f [y, x] 2 z rest'))))

      fun merge f l1 l2 =
          (case (l1, l2) of
              ([], _) =>
                 l2
            | (_, []) =>
                 l1
            | (x1 :: rest1, x2 :: rest2) =>
                 (case f (x1, x2) of
                     GREATER =>
                        x2 :: merge f l1 rest2
                   | _ =>
                        x1 :: merge f rest1 l2))


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


       (* grow f s ssz p u req
          
          If    s is sorted
                ssz = |s|
                p is a power of 2
                p <= ssz  or  u=[]
                req > 0
          then  u = u1 @ u2
                l contains the same elements as s @ u1
                l is sorted
                |l| >= min(req, |s|+|u|)
                (so |l| >= req  or  u2=[])
                and
                return (l, |l|, u2)
       *)
       fun grow f s ssz p u req =
          if req <= ssz then
             (s, ssz, u)
          else
             (case u of
                 [] =>
                    (s, ssz, u)
               | h :: rest =>
                    let
                       val (r, rsz, x) = getrun f h rest
       
                       val p' = roundpow p ssz
                       (* p' is the greatest power of 2 <= ssz *)
       
                       val (t, tsz, y) = grow f r rsz 1 x p'
                       (* tsz >= p' unless u=[], so 2 * p' <= ssz+tsz unless u=[] *)
                    in
                       grow f (merge f s t) (ssz + tsz) (p' + p') y req 
                    end)


       fun sort f l =
          (case l of
              [] => []
            | h :: rest =>
                 let
                    val (r, rsz, x) = getrun f h rest

                    val (l', _, _) = grow f r rsz 1 x infinity
                 in
                    l'
                 end)

   end

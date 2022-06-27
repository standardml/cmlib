
functor ListDict (structure Key : ORDERED)
   :> DICT where type key = Key.t
   =
   struct

      type key = Key.t
      type 'a dict = (key * 'a) list

      exception Absent

      val empty = []

      val isEmpty = List.null

      val size = List.length

      fun singleton key x = [(key, x)]

      fun insert l key x =
         (case l of
             [] => [(key, x)]
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       (key, x) :: l
                  | EQUAL =>
                       (key, x) :: rest
                  | GREATER =>
                       (key', y) :: insert rest key x))

      fun insert' l key x =
         (case l of
             [] =>
                ([(key, x)], false)
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       ((key, x) :: l, false)
                  | EQUAL =>
                       ((key, x) :: rest, true)
                  | GREATER =>
                       let
                          val (d, present) = insert' rest key x
                       in
                          ((key', y) :: d, present)
                       end))

      fun remove l key =
         (case l of
             [] => []
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS => l
                  | EQUAL => rest
                  | GREATER =>
                       (key', y) :: remove rest key))

      fun remove' l key =
         (case l of
             [] => ([], false)
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS => (l, false)
                  | EQUAL => (rest, true)
                  | GREATER =>
                       let
                          val (d, present) = remove' rest key
                       in
                          ((key', y) :: d, present)
                       end))

      fun operate' l key absentf presentf =
         (case l of
             [] =>
                (case absentf () of
                    NONE =>
                       (NONE, NONE, [])
                  | z as SOME x =>
                       (NONE, z, [(key, x)]))
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       (case absentf () of
                           NONE =>
                              (NONE, NONE, l)
                         | z as SOME x =>
                              (NONE, z, (key, x) :: l))
                  | EQUAL =>
                       (case presentf y of
                           NONE =>
                              (SOME y, NONE, rest)
                         | z as SOME x =>
                              (SOME y, z, (key, x) :: rest))
                  | GREATER =>
                       let
                          val (ante, post, rest') = operate' rest key absentf presentf
                       in
                          (ante, post, (key', y) :: rest')
                       end))

      fun operate dict key absentf presentf =
         let
            val (x, y, d) = operate' dict key (SOME o absentf) (SOME o presentf)
         in
            (x, valOf y, d)
         end
         
      fun insertMerge dict key x f =
         let
            val (_, _, z) =
               operate' dict key (fn () => SOME x) (SOME o f)
         in
            z
         end

      fun find l key =
         (case l of
             [] => 
                NONE
           | (key', x) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       NONE
                  | EQUAL =>
                       SOME x
                  | GREATER =>
                       find rest key))

      fun lookup l key =
         (case l of
             [] => 
                raise Absent
           | (key', x) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       raise Absent
                  | EQUAL =>
                       x
                  | GREATER =>
                       lookup rest key))

      fun member l key =
         (case l of
             [] =>
                false
           | (key', _) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       false
                  | EQUAL =>
                       true
                  | GREATER =>
                       member rest key))

      fun toList l = l

      fun domain l = List.map (fn (key, _) => key) l

      fun map f l = List.map (fn (key, x) => (key, f x)) l

      fun map' f l = List.map (fn (kx as (key, x)) => (key, f kx)) l

      fun foldl f base l = List.foldl (fn ((key, x), y) => f (key, x, y)) base l

      fun foldr f base l = List.foldr (fn ((key, x), y) => f (key, x, y)) base l

      val app = List.app

      fun union l1 l2 merge =
         (case (l1, l2) of
             (nil, _) => l2

           | (_, nil) => l1

           | ((key1, x) :: rest1, (key2, y) :: rest2) =>
                (case Key.compare (key1, key2) of
                    LESS =>
                       (key1, x) :: union rest1 l2 merge

                  | EQUAL =>
                       (key1, merge (key1, x, y)) :: union rest1 rest2 merge

                  | GREATER =>
                       (key2, y) :: union l1 rest2 merge))


      fun split key acc l =
         (case l of
             nil =>
                (acc, NONE, nil)

           | (item as (key', _)) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       (acc, NONE, l)

                  | EQUAL =>
                       (acc, SOME item, rest)

                  | GREATER =>
                       split key (item :: acc) rest))

      fun partition l key =
         (case split key nil l of
             (leftr, NONE, right) => (rev leftr, NONE, right)

           | (leftr, SOME (_, x), right) => (rev leftr, SOME x, right))

      fun partitionlt l key =
         (case split key nil l of
             (leftr, NONE, right) => (rev leftr, right)

           | (leftr, SOME item, right) => (rev leftr, item :: right))

      fun partitiongt l key =
         (case split key nil l of
             (leftr, NONE, right) => (rev leftr, right)

           | (leftr, SOME item, right) => (rev (item :: leftr), right))

      fun rangeii tree left right =
         let
            val (_, tree') = partitionlt tree left
            val (tree'', _) = partitiongt tree' right
         in
            tree''
         end

      fun rangeie tree left right =
         let
            val (_, tree') = partitionlt tree left
            val (tree'', _) = partitionlt tree' right
         in
            tree''
         end

      fun rangeei tree left right =
         let
            val (_, tree') = partitiongt tree left
            val (tree'', _) = partitiongt tree' right
         in
            tree''
         end

      fun rangeee tree left right =
         let
            val (_, tree') = partitiongt tree left
            val (tree'', _) = partitionlt tree' right
         in
            tree''
         end

      fun least l =
         (case l of
             nil => raise Absent

           | (key, x) :: _ => (key, x))

      fun greatest l =
         (case l of
             nil => raise Absent
           | [(key, x)] => (key, x)
           | _ :: t => greatest t)

      fun leastGt l key =
         (case l of
             nil => raise Absent

           | (key', x) :: rest =>
                (case Key.compare (key', key) of
                    GREATER => (key', x)

                  | _ => leastGt rest key))

      fun leastGeq l key =
         (case l of
             nil => raise Absent

           | (key', x) :: rest =>
                (case Key.compare (key', key) of
                    LESS => leastGeq rest key

                  | _ => (key', x)))

      fun greatestLt l key =
         let
            fun loop prev l =
               (case l of
                   nil => prev

                 | (key', x) :: rest =>
                      (case Key.compare (key', key) of
                          LESS =>
                             loop (key', x) rest

                        | _ => prev))
         in
            (case l of
                nil => raise Absent

              | (key', x) :: rest =>
                   (case Key.compare (key', key) of
                       LESS =>
                          loop (key', x) rest

                     | _ => raise Absent))
         end

      fun greatestLeq l key =
         let
            fun loop prev l =
               (case l of
                   nil => prev

                 | (key', x) :: rest =>
                      (case Key.compare (key', key) of
                          GREATER => prev

                        | _ => loop (key', x) rest))
         in
            (case l of
                nil => raise Absent

              | (key', x) :: rest =>
                   (case Key.compare (key', key) of
                       GREATER => raise Absent

                     | _ => loop (key', x) rest))
         end

   end

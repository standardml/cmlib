
functor SplayDict (structure Key : ORDERED)
   :>
   DICT where type key = Key.t
   =
   struct

      type key = Key.t

      open SplayTree

      type 'a dict = (key * 'a) tree

      exception Absent

      val empty = Leaf


      fun singleton key datum =
         Node (ref (1, (key, datum), Leaf, Leaf))


      fun insert tree key datum =
         (case tree of
             Leaf =>
                singleton key datum

           | Node root =>
                let
                   val (order, node' as (_, label, left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          mknoder ((key, datum), left, right)

                     | LESS =>
                          mknoder ((key, datum), left, mknoder (label, Leaf, right))

                     | GREATER =>
                          mknoder ((key, datum), mknoder (label, left, Leaf), right))
                end)


      fun insert' tree key datum =
         (case tree of
             Leaf =>
                (singleton key datum, false)

           | Node root =>
                let
                   val (order, node' as (_, label, left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          (mknoder ((key, datum), left, right), true)

                     | LESS =>
                          (mknoder ((key, datum), left, mknoder (label, Leaf, right)), false)

                     | GREATER =>
                          (mknoder ((key, datum), mknoder (label, left, Leaf), right), false))
                end)


      fun remove tree key =
         (case tree of
             Leaf => empty

           | Node root =>
                let
                   val (order, (_, _, left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          join left right

                     | _ => tree)
                end)


      fun remove' tree key =
         (case tree of
             Leaf =>
                (empty, false)
           | Node root =>
                let
                   val (order, (_, _, left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          (join left right, true)

                     | _ => 
                          (tree, false))
                end)


      fun operate' tree key absentf presentf =
         (case tree of
             Leaf =>
                (case absentf () of
                    NONE =>
                       (NONE, NONE, Leaf)
                  | y as SOME datum =>
                       (NONE, y, singleton key datum))

           | Node root =>
                let
                   val (order, (_, label as (key', datum'), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          (case presentf datum' of
                              NONE =>
                                 (SOME datum', NONE, join left right)

                            | y as SOME datum =>
                                 (SOME datum', y,
                                  mknoder ((key, datum), left, right)))

                     | LESS =>
                          (case absentf () of
                              NONE =>
                                 (NONE, NONE, mknoder (label, left, right))

                            | y as SOME datum =>
                                 (NONE, y,
                                  mknoder ((key, datum), left, mknoder (label, Leaf, right))))

                     | GREATER =>
                          (case absentf () of
                              NONE =>
                                 (NONE, NONE, mknoder (label, left, right))

                            | y as SOME datum =>
                                 (NONE, y,
                                  mknoder ((key, datum), mknoder (label, left, Leaf), right))))
                end)


      fun operate dict key absentf presentf =
         let
            val (x, y, d) = operate' dict key (SOME o absentf) (SOME o presentf)
         in
            (x, valOf y, d)
         end


      fun insertMerge dict key x f =
         let
            val (_, _, y) = operate' dict key (fn () => SOME x) (SOME o f)
         in
            y
         end


      fun find tree key =
         (case tree of
             Leaf => NONE

           | Node root =>
                (case findAndSplay (fn (key', _) => Key.compare (key, key')) root [] of
                    (EQUAL, (_, (_, datum), _, _)) =>
                       SOME datum

                  | _ => NONE))


      fun lookup tree key =
         (case tree of
             Leaf => raise Absent

           | Node root =>
                (case findAndSplay (fn (key', _) => Key.compare (key, key')) root [] of
                    (EQUAL, (_, (_, datum), _, _)) =>
                       datum

                  | _ =>
                       raise Absent))

      fun isEmpty d =
         (case d of
             Leaf => true
           | Node _ => false)


      fun member tree key =
         (case tree of
             Leaf => false

           | Node root =>
                (case findAndSplay (fn (key', _) => Key.compare (key, key')) root [] of
                    (EQUAL, _) => true
                  | _ => false))


      fun foldl f x tree =
         (case tree of
             Leaf => x
           | Node (ref (_, (key, datum), left, right)) =>
                foldl f (f (key, datum, foldl f x left)) right)


      fun foldr f x tree =
         (case tree of
             Leaf => x
           | Node (ref (_, (key, datum), left, right)) =>
                foldr f (f (key, datum, foldr f x right)) left)


      fun toList tree =
         foldr (fn (key, datum, l) => (key, datum) :: l) [] tree


      fun domain tree =
         foldr (fn (key, _, l) => key :: l) [] tree


      fun map f tree =
         (case tree of
             Leaf => Leaf
           | Node (ref (sz, (key, datum), left, right)) =>
                Node (ref (sz, (key, f datum), map f left, map f right)))


      fun app f tree =
         (case tree of
             Leaf => ()
           | Node (ref (_, label, left, right)) =>
                (
                app f left;
                f label;
                app f right
                ))



      fun partition tree key =
         (case tree of
             Leaf => (Leaf, NONE, Leaf)

           | Node root =>
                let 
                   val (left, middle, right) = 
                      split (fn (key', _) => Key.compare (key, key')) root
                in
                   (left, Option.map (fn (_, datum) => datum) middle, right)
                end)


      fun partitionlt tree key =
         (case tree of
             Leaf => (Leaf, Leaf)

           | Node root =>
                let 
                   val (left, _, right) = 
                      split 
                         (fn (key', _) => 
                             (case Key.compare (key, key') of
                                 GREATER => GREATER
                               | _ => LESS))
                         root
                in
                   (left, right)
                end)


      fun partitiongt tree key =
         (case tree of
             Leaf => (Leaf, Leaf)

           | Node root =>
                let 
                   val (left, _, right) = 
                      split 
                         (fn (key', _) => 
                             (case Key.compare (key, key') of
                                 LESS => LESS
                               | _ => GREATER))
                         root
                in
                   (left, right)
                end)


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


      fun least tree =
         (case tree of
             Leaf =>
                raise Absent

           | Node root =>
                let
                   val ((_, datum), _) = splayMin root
                in
                   datum
                end)


      fun greatest tree =
         (case tree of
             Leaf =>
                raise Absent

           | Node root =>
                let
                   val ((_, datum), _) = splayMax root
                in
                   datum
                end)


      fun leastGt tree key =
         (case tree of
             Leaf =>
                raise Absent

           | Node root =>
                let
                   val (order, (_, (_, datum), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       GREATER => datum
                     | _ => least right)
                end)


      fun leastGeq tree key =
         (case tree of
             Leaf =>
                raise Absent

           | Node root =>
                let
                   val (order, (_, (_, datum), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       LESS => least right
                     | _ => datum)
                end)


      fun greatestLt tree key =
         (case tree of
             Leaf =>
                raise Absent

           | Node root =>
                let
                   val (order, (_, (_, datum), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       LESS => datum
                     | _ => greatest left)
                end)


      fun greatestLeq tree key =
         (case tree of
             Leaf =>
                raise Absent

           | Node root =>
                let
                   val (order, (_, (_, datum), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       GREATER => greatest left
                     | _ => datum)
                end)


      fun union tree1 tree2 merge =
         (case tree1 of
             Leaf =>
                tree2

           | Node (ref (_, label1 as (key1, datum1), left1, right1)) =>
                (case tree2 of
                    Leaf =>
                       tree1

                  | Node root2 =>
                       let
                          val (left2, middle, right2) =
                             split (fn (key2, _) => Key.compare (key1, key2)) root2

                          val label =
                             (case middle of
                                 SOME (_, datum2) => (key1, merge (key1, datum1, datum2))
                               | NONE => label1)
                       in
                          mknoder (label, union left1 left2 merge, union right1 right2 merge)
                       end))

   end

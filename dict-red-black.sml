
functor RedBlackDict (structure Key : ORDERED)
   :> DICT where type key = Key.t
   =
   struct

      type key = Key.t
      
      open RedBlackTree

      type 'a dict = (key * 'a) tree

      exception Absent

      val empty = Leaf

      fun singleton key datum =
         Node (BLACK, 1, (key, datum), Leaf, Leaf)

      fun isEmpty d =
         (case d of
             Leaf => true
           | Node _ => false)

      fun insert tree key datum =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, zipper) =>
                zipRed ((key, datum), Leaf, Leaf) zipper

           | (Node (color, sz, _, left, right), zipper) =>
                zip (Node (color, sz, (key, datum), left, right)) zipper)

      fun insert' tree key datum =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, zipper) =>
                (zipRed ((key, datum), Leaf, Leaf) zipper, false)

           | (Node (color, sz, _, left, right), zipper) =>
                (zip (Node (color, sz, (key, datum), left, right)) zipper, true))

      fun remove tree key =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, _) =>
                tree

           | (Node (color, _, _, left, right), zipper) =>
                delete color left right zipper)

      fun remove' tree key =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, _) =>
                (tree, false)

           | (Node (color, _, _, left, right), zipper) =>
                (delete color left right zipper, true))

      fun member tree key =
         (case tree of
             Leaf => false

           | Node (_, _, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    EQUAL =>
                       true
                  | LESS =>
                       member left key
                  | GREATER =>
                       member right key))

      fun find tree key =
         (case tree of
             Leaf => NONE
           | Node (_, _, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    EQUAL =>
                       SOME datum
                  | LESS =>
                       find left key
                  | GREATER =>
                       find right key))

      fun lookup tree key =
         (case tree of
             Leaf =>
                raise Absent
           | Node (_, _, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    EQUAL =>
                       datum
                  | LESS =>
                       lookup left key
                  | GREATER =>
                       lookup right key))

      fun operate' tree key absentf presentf =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, zipper) =>
                (case absentf () of
                    NONE =>
                       (NONE, NONE, tree)

                  | y as SOME datum =>
                       (NONE, y,
                        zipRed ((key, datum), Leaf, Leaf) zipper))

           | (Node (color, sz, (_, datum), left, right), zipper) =>
                (case presentf datum of
                    NONE =>
                       (SOME datum, NONE,
                        delete color left right zipper)

                  | y as SOME datum' =>
                       (SOME datum, y,
                        zip (Node (color, sz, (key, datum'), left, right)) zipper)))

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

      fun foldl f x tree =
         (case tree of
             Leaf => x
           | Node (_, _, (key, elem), left, right) =>
                foldl f (f (key, elem, foldl f x left)) right)

      fun foldr f x tree =
         (case tree of
             Leaf => x
           | Node (_, _, (key, elem), left, right) =>
                foldr f (f (key, elem, foldr f x right)) left)

      fun toList tree = foldr (fn (key, datum, l) => (key, datum) :: l) [] tree

      fun domain tree = foldr (fn (key, _, l) => key :: l) [] tree
      
      fun map f tree =
         (case tree of
             Leaf => Leaf
           | Node (color, sz, (key, datum), left, right) =>
                Node (color, sz, (key, f datum), map f left, map f right))

      fun app f tree =
         (case tree of
             Leaf => ()
           | Node (_, _, label, left, right) =>
                (
                app f left;
                f label;
                app f right
                ))

      fun union tree1 tree2 merge =
         (case tree1 of
             Leaf => tree2

           | Node (_, _, (label1 as (key1, datum1)), left1, right1) =>
                (case tree2 of
                    Leaf => tree1

                  | _ =>
                       let
                          val (labelo2, left2, right2) =
                             split (fn (key2, _) => Key.compare (key1, key2)) tree2

                          val label =
                             (case labelo2 of
                                 NONE => label1
                               | SOME (_, datum2) => (key1, merge (key1, datum1, datum2)))
                       in
                          join label (union left1 left2 merge) (union right1 right2 merge)
                       end))

      fun partition tree key =
         let
            val (labelo, left, right) =
               split (fn (key', _) => Key.compare (key, key')) tree
         in
            (left, Option.map (fn (_, x) => x) labelo, right)
         end

      fun partitionlt tree key =
         let
            val (_, left, right) =
               split 
                  (fn (key', _) => 
                      (case Key.compare (key, key') of
                          GREATER => GREATER
                        | _ => LESS))
                  tree
         in
            (left, right)
         end

      fun partitiongt tree key =
         let
            val (_, left, right) =
               split 
                  (fn (key', _) => 
                      (case Key.compare (key, key') of
                          LESS => LESS
                        | _ => GREATER))
                  tree
         in
            (left, right)
         end

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
             Leaf => raise Absent

           | Node (_, _, (_, x), Leaf, _) => x

           | Node (_, _, _, left, _) => least left)

      fun greatest tree =
         (case tree of
             Leaf => raise Absent

           | Node (_, _, (_, x), _, Leaf) => x

           | Node (_, _, _, _, right) => greatest right)

      fun leastGt tree key =
         (case tree of
             Leaf => raise Absent

           | Node (_, _, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    LESS =>
                       (leastGt left key
                        handle Absent => datum)

                  | EQUAL => least right

                  | GREATER => leastGt right key))

      fun leastGeq tree key =
         (case tree of
             Leaf => raise Absent

           | Node (_, _, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    LESS =>
                       (leastGeq left key
                        handle Absent => datum)

                  | EQUAL => datum

                  | GREATER => leastGeq right key))

      fun greatestLt tree key =
         (case tree of
             Leaf => raise Absent

           | Node (_, _, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    LESS => greatestLt left key

                  | EQUAL => greatest left

                  | GREATER => 
                       (greatestLt right key
                        handle Absent => datum)))

      fun greatestLeq tree key =
         (case tree of
             Leaf => raise Absent

           | Node (_, _, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    LESS => greatestLeq left key

                  | EQUAL => datum

                  | GREATER => 
                       (greatestLeq right key
                        handle Absent => datum)))

   end


functor RedBlackPreDict (structure Key : ORDERED)
   :> PRE_DICT where type key = Key.t
   =
   struct

      type key = Key.t
      
      open RedBlackTree

      type 'a dict = (key * 'a) tree

      exception Absent

      val empty = Leaf

      fun singleton key datum =
         Node (BLACK, (key, datum), Leaf, Leaf)

      fun isEmpty d =
         (case d of
             Leaf => true
           | Node _ => false)

      fun insert tree key datum =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, zipper) =>
                zipRed ((key, datum), Leaf, Leaf) zipper
           | (Node (color, _, left, right), zipper) =>
                zip (Node (color, (key, datum), left, right)) zipper)

      fun insert' tree key datum =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, zipper) =>
                (zipRed ((key, datum), Leaf, Leaf) zipper, false)
           | (Node (color, _, left, right), zipper) =>
                (zip (Node (color, (key, datum), left, right)) zipper, true))

      fun remove tree key =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, _) =>
                tree
           | (Node (color, _, left, right), zipper) =>
                delete color left right zipper)

      fun remove' tree key =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, _) =>
                (tree, false)
           | (Node (color, _, left, right), zipper) =>
                (delete color left right zipper, true))

      fun member tree key =
         (case tree of
             Leaf => false
           | Node (_, (key', datum), left, right) =>
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
           | Node (_, (key', datum), left, right) =>
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
           | Node (_, (key', datum), left, right) =>
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
           | (Node (color, (_, datum), left, right), zipper) =>
                (case presentf datum of
                    NONE =>
                       (SOME datum, NONE,
                        delete color left right zipper)
                  | y as SOME datum' =>
                       (SOME datum, y,
                        zip (Node (color, (key, datum'), left, right)) zipper)))

      fun operate dict key absentf presentf =
         let
            val (x, y, d) = operate' dict key (SOME o absentf) (SOME o presentf)
         in
            (x, valOf y, d)
         end
         
      fun insertMerge dict key x f =
         #3 (operate' dict key (fn () => SOME x) (SOME o f))

      fun foldl f x tree =
         (case tree of
             Leaf => x
           | Node (_, (key, elem), left, right) =>
                foldl f (f (key, elem, foldl f x left)) right)

      fun foldr f x tree =
         (case tree of
             Leaf => x
           | Node (_, (key, elem), left, right) =>
                foldr f (f (key, elem, foldr f x right)) left)

      fun toList tree = foldr (fn (key, datum, l) => (key, datum) :: l) [] tree

      fun domain tree = foldr (fn (key, _, l) => key :: l) [] tree
      
      fun map f tree =
         (case tree of
             Leaf => Leaf
           | Node (color, (key, datum), left, right) =>
                Node (color, (key, f datum), map f left, map f right))

      fun app f tree =
         (case tree of
             Leaf => ()
           | Node (_, label, left, right) =>
                (
                app f left;
                f label;
                app f right
                ))

   end


functor RedBlackDict (structure Key : ORDERED)
   :>
   DICT where type key = Key.t
   =
   DictFun (RedBlackPreDict (structure Key = Key))

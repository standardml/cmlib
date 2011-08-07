
functor RedBlackDict (structure Key : ORDERED)
   :> DICT where type key = Key.t
   =
   struct

      type key = Key.t
      
      open RedBlackTree

      type 'a dict = int * (key * 'a) tree

      exception Absent

      val empty = (0, Leaf)

      fun singleton key datum =
         (1, Node (BLACK, (key, datum), Leaf, Leaf))

      fun isEmpty (n, _) = n = 0

      fun size (n, _) = n

      fun insert (n, tree) key datum =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, zipper) =>
                (n+1, zipRed ((key, datum), Leaf, Leaf) zipper)
           | (Node (color, _, left, right), zipper) =>
                (n, zip (Node (color, (key, datum), left, right)) zipper))

      fun remove (dict as (n, tree)) key =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, _) => dict
           | (Node (color, _, left, right), zipper) =>
                (n-1, delete color left right zipper))

      fun memberMain tree key =
         (case tree of
             Leaf => false
           | Node (_, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    EQUAL =>
                       true
                  | LESS =>
                       memberMain left key
                  | GREATER =>
                       memberMain right key))

      fun member (n, tree) key =
         memberMain tree key

      fun findMain tree key =
         (case tree of
             Leaf => NONE
           | Node (_, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    EQUAL =>
                       SOME datum
                  | LESS =>
                       findMain left key
                  | GREATER =>
                       findMain right key))

      fun find (n, tree) key =
         findMain tree key

      fun lookupMain tree key =
         (case tree of
             Leaf =>
                raise Absent
           | Node (_, (key', datum), left, right) =>
                (case Key.compare (key, key') of
                    EQUAL =>
                       datum
                  | LESS =>
                       lookupMain left key
                  | GREATER =>
                       lookupMain right key))

      fun lookup (_, tree) key =
         lookupMain tree key

      fun operate (n, tree) key absentf presentf =
         (case search (fn (key', _) => Key.compare (key, key')) tree [] of
             (Leaf, zipper) =>
                let
                   val datum = absentf ()
                in
                   (NONE, datum,
                    (n+1, zipRed ((key, datum), Leaf, Leaf) zipper))
                end
           | (Node (color, (_, datum), left, right), zipper) =>
                let
                   val datum' = presentf datum
                in
                   (SOME datum, datum',
                    (n, zip (Node (color, (key, datum'), left, right)) zipper))
                end)

      fun insertMerge dict key x f =
         #3 (operate dict key (fn () => x) f)

      fun foldlMain f x tree =
         (case tree of
             Leaf => x
           | Node (_, (key, elem), left, right) =>
                foldlMain f (f (key, elem, foldlMain f x left)) right)

      fun foldrMain f x tree =
         (case tree of
             Leaf => x
           | Node (_, (key, elem), left, right) =>
                foldrMain f (f (key, elem, foldrMain f x right)) left)

      fun foldl f x (_, tree) = foldlMain f x tree

      fun foldr f x (_, tree) = foldrMain f x tree

      fun toList (_, tree) = foldrMain (fn (key, datum, l) => (key, datum) :: l) [] tree

      fun domain (_, tree) = foldrMain (fn (key, _, l) => key :: l) [] tree
      
      fun mapMain f tree =
         (case tree of
             Leaf => Leaf
           | Node (color, (key, datum), left, right) =>
                Node (color, (key, f datum), mapMain f left, mapMain f right))

      fun map f (n, tree) =
         (n, mapMain f tree)

      fun appMain f tree =
         (case tree of
             Leaf => ()
           | Node (_, label, left, right) =>
                (
                appMain f left;
                f label;
                appMain f right
                ))

      fun app f (_, tree) =
         appMain f tree

      fun union (dict1 as (n1, tree1)) (dict2 as (n2, tree2)) f =
         if n1 <= n2 then
            foldlMain
            (fn (key, datum, dict) =>
                insertMerge dict key datum
                (fn datum' => f (key, datum, datum')))
            dict2
            tree1
         else
            foldlMain
            (fn (key, datum, dict) =>
                insertMerge dict key datum
                (fn datum' => f (key, datum', datum)))
            dict1
            tree2

   end

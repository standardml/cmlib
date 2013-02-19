
functor RedBlackSet (structure Elem : ORDERED)
   :> SET where type elem = Elem.t
   =
   struct

      type elem = Elem.t

      open RedBlackTree

      type set = int * elem tree

      val empty = (0, Leaf)

      fun size (n, _) = n

      fun isEmpty (n, _) = n = 0

      fun singleton elem =
         (1, Node (BLACK, elem, Leaf, Leaf))

      fun insert (set as (n, tree)) elem =
         (case search (fn elem' => Elem.compare (elem, elem')) tree [] of
             (Leaf, zipper) =>
                (n+1, zipRed (elem, Leaf, Leaf) zipper)
           | (Node _, _) => set)

      fun remove (set as (n, tree)) elem =
         (case search (fn elem' => Elem.compare (elem, elem')) tree [] of
             (Leaf, _) => set
           | (Node (color, _, left, right), zipper) =>
                (n-1, delete color left right zipper))

      fun memberMain tree elem =
         (case tree of
             Leaf => false
           | Node (_, elem', left, right) =>
                (case Elem.compare (elem, elem') of
                    EQUAL => true
                  | LESS =>
                       memberMain left elem
                  | GREATER =>
                       memberMain right elem))

      fun member (_, tree) elem =
         memberMain tree elem

      fun foldlMain f x tree =
         (case tree of
             Leaf => x
           | Node (_, elem, left, right) =>
                foldlMain f (f (elem, foldlMain f x left)) right)

      fun foldrMain f x tree =
         (case tree of
             Leaf => x
           | Node (_, elem, left, right) =>
                foldrMain f (f (elem, foldrMain f x right)) left)

      fun foldl f x (_, tree) = foldlMain f x tree

      fun foldr f x (_, tree) = foldrMain f x tree

      fun toList (_, tree) = foldrMain (op ::) [] tree

      fun appMain f tree =
         (case tree of
             Leaf => ()
           | Node (_, elem, left, right) =>
                (
                appMain f left;
                f elem;
                appMain f right
                ))

      fun app f (_, tree) = appMain f tree

      fun union (set1 as (n1, tree1)) (set2 as (n2, tree2)) =
         if n1 <= n2 then
            foldlMain
            (fn (elem, set) => insert set elem)
            set2
            tree1
         else
            foldlMain
            (fn (elem, set) => insert set elem)
            set1
            tree2

      fun intersection (set1 as (n1, tree1)) (set2 as (n2, tree2)) =
         if n1 <= n2 then
            foldlMain
            (fn (elem, set) =>
                if member set2 elem then
                   insert set elem
                else
                   set)
            empty
            tree1
         else
            foldlMain
            (fn (elem, set) =>
                if member set1 elem then
                   insert set elem
                else
                   set)
            empty
            tree2

      fun difference (set1 as (n1, tree1)) (set2 as (n2, tree2)) =
         foldlMain
         (fn (elem, set) =>
             if member set2 elem then
                set
             else
                insert set elem)
         empty
         tree1

      fun subsetMain tree1 tree2 =
         (case tree1 of
             Leaf => true
           | Node (_, elem1, left1, right1) =>
                (case tree2 of
                    Leaf => false
                  | Node (_, elem2, left2, right2) =>
                       (case Elem.compare (elem1, elem2) of
                           EQUAL =>
                              subsetMain left1 left2
                              andalso
                              subsetMain right1 right2
                         | LESS =>
                              memberMain left2 elem1
                              andalso
                              subsetMain left1 left2
                              andalso
                              subsetMain right1 tree2
                         | GREATER =>
                              memberMain right2 elem1
                              andalso
                              subsetMain right1 right2
                              andalso
                              subsetMain left1 tree2)))

      fun subset ((_, tree1), (_, tree2)) =
         subsetMain tree1 tree2

      fun eq (set1, set2) =
         subset (set1, set2)
         andalso
         subset (set2, set1)

   end

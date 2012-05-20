
functor SplaySet (structure Elem : ORDERED)
   :> SET where type elem = Elem.t
   =
   struct

      type elem = Elem.t

      open SplayTree

      type set = int * elem tree

      val empty = (0, Leaf)

      fun isEmpty (n, _) = n = 0

      fun singleton elem =
         (1, Node (ref (elem, Leaf, Leaf)))

      fun insert (set as (n, tree)) elem =
         (case tree of
             Leaf =>
                singleton elem
           | Node root =>
                let
                   val (order, (elem', left, right)) =
                      findAndSplay (fn elem' => Elem.compare (elem, elem')) root []
                in
                   (case order of
                       EQUAL => set
                     | LESS =>
                          (n+1,
                           Node (ref (elem, left, Node (ref (elem', Leaf, right)))))
                     | GREATER =>
                          (n+1,
                           Node (ref (elem, Node (ref (elem', left, Leaf)), right))))
                end)

      fun remove (set as (n, tree)) elem =
         (case tree of
             Leaf =>
                empty
           | Node root =>
                let
                   val (order, (_, left, right)) =
                      findAndSplay (fn elem' => Elem.compare (elem, elem')) root []
                in
                   (case order of
                       EQUAL =>
                          (n-1, join left right)
                     | _ => set)
                end)

      fun member (_, tree) elem =
         (case tree of
             Leaf => false
           | Node root =>
                (case findAndSplay (fn elem' => Elem.compare (elem, elem')) root [] of
                    (EQUAL, _) => true
                  | _ => false))

      fun unionMain tree1 tree2 =
         (case tree1 of
             Leaf =>
                (SplayTree.size tree2, tree2)
           | Node (ref (elem1, left1, right1)) =>
                (case tree2 of
                    Leaf =>
                       (SplayTree.size tree1, tree1)
                  | Node root2 =>
                       let
                          val (order, left2, right2) =
                             split (fn elem' => Elem.compare (elem1, elem')) root2

                          val (nleft, left') = unionMain left1 left2
                          val (nright, right') = unionMain right1 right2
                       in
                          (1+nleft+nright,
                           Node (ref (elem1, left', right')))
                       end))

      fun union (set1 as (n1, tree1)) (set2 as (n2, tree2)) =
         if n1 = 0 then
            set2
         else if n2 = 0 then
            set1
         else if n1 >= n2 then
            unionMain tree1 tree2
         else
            unionMain tree2 tree1

      fun intersectionMain tree1 tree2 =
         (case tree1 of
             Leaf =>
                (0, Leaf)
           | Node (ref (elem1, left1, right1)) =>
                (case tree2 of
                    Leaf =>
                       (0, Leaf)
                  | Node root2 =>
                       let
                          val (order, left2, right2) =
                             split (fn elem' => Elem.compare (elem1, elem')) root2

                          val (nleft, left') = intersectionMain left1 left2
                          val (nright, right') = intersectionMain right1 right2
                       in
                          (case order of
                              EQUAL =>
                                 (1+nleft+nright,
                                  Node (ref (elem1, left', right')))
                            | _ =>
                                 (nleft+nright, join left' right'))
                       end))

      fun intersection (set1 as (n1, tree1)) (set2 as (n2, tree2)) =
         if n1 = 0 then
            (0, Leaf)
         else if n2 = 0 then
            (0, Leaf)
         else if n1 >= n2 then
            intersectionMain tree1 tree2
         else
            intersectionMain tree2 tree1

      fun differenceMain tree1 tree2 =
         (case tree1 of
             Leaf =>
                (0, Leaf)
           | Node (ref (elem1, left1, right1)) =>
                (case tree2 of
                    Leaf =>
                       (SplayTree.size tree1, tree1)
                  | Node root2 =>
                       let
                          val (order, left2, right2) =
                             split (fn elem' => Elem.compare (elem1, elem')) root2

                          val (nleft, left') = differenceMain left1 left2
                          val (nright, right') = differenceMain right1 right2
                       in
                          (case order of
                              EQUAL =>
                                 (nleft+nright, join left' right')
                            | _ =>
                                 (1+nleft+nright,
                                  Node (ref (elem1, left', right'))))
                       end))

      fun difference (set1 as (n1, tree1)) (set2 as (n2, tree2)) =
         if n1 = 0 then
            (0, Leaf)
         else if n2 = 0 then
            set1
         else
            differenceMain tree1 tree2

      fun eqMain tree1 tree2 =
         (case (tree1, tree2) of
             (Leaf, Leaf) => true
           | (Leaf, Node _) => false
           | (Node _, Leaf) => false
           | (Node (ref (elem1, left1, right1)), Node root2) =>
                let
                   val (order, left2, right2) =
                      split (fn elem' => Elem.compare (elem1, elem')) root2
                in
                   (case order of
                       EQUAL =>
                          eqMain left1 left2
                          andalso
                          eqMain right1 right2
                     | _ => false)
                end)

      fun eq ((_, tree1), (_, tree2)) = eqMain tree1 tree2

      fun subsetMain tree1 tree2 =
         (case (tree1, tree2) of
             (Leaf, Leaf) => true
           | (Leaf, Node _) => true
           | (Node _, Leaf) => false
           | (Node (ref (elem1, left1, right1)), Node root2) =>
                let
                   val (order, left2, right2) =
                      split (fn elem' => Elem.compare (elem1, elem')) root2
                in
                   (case order of
                       EQUAL =>
                          subsetMain left1 left2
                          andalso
                          subsetMain right1 right2
                     | _ => false)
                end)         

      fun subset ((_, tree1), (_, tree2)) = subsetMain tree1 tree2

      fun size (n, _) = n

      fun foldlMain f x tree =
         (case tree of
             Leaf => x
           | Node (ref (elem, left, right)) =>
                foldlMain f (f (elem, foldlMain f x left)) right)

      fun foldrMain f x tree =
         (case tree of
             Leaf => x
           | Node (ref (elem, left, right)) =>
                foldrMain f (f (elem, foldrMain f x right)) left)

      fun toList (_, tree) = foldrMain (op ::) [] tree

      fun foldl f x (_, tree) = foldlMain f x tree

      fun foldr f x (_, tree) = foldrMain f x tree

      fun appMain f tree =
         (case tree of
             Leaf => ()
           | Node (ref (elem, left, right)) =>
                (
                appMain f left;
                f elem;
                appMain f right
                ))

      fun app f (_, tree) = appMain f tree
         
   end


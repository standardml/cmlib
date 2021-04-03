
functor RedBlackSet (structure Elem : ORDERED)
   :> SET where type elem = Elem.t
   =
   struct

      type elem = Elem.t

      open RedBlackTree

      type set = elem tree


      val empty = Leaf


      fun isEmpty d =
         (case d of
             Leaf => true
           | Node _ => false)


      fun singleton elem =
         Node (BLACK, 1, elem, Leaf, Leaf)


      fun insert tree elem =
         (case search (fn elem' => Elem.compare (elem, elem')) tree [] of
             (Leaf, zipper) =>
                zipRed (elem, Leaf, Leaf) zipper

           | (Node _, _) => tree)


      fun remove tree elem =
         (case search (fn elem' => Elem.compare (elem, elem')) tree [] of
             (Leaf, _) => tree

           | (Node (color, _, _, left, right), zipper) =>
                delete color left right zipper)


      fun member tree elem =
         (case tree of
             Leaf => false

           | Node (_, _, elem', left, right) =>
                (case Elem.compare (elem, elem') of
                    EQUAL => true

                  | LESS =>
                       member left elem

                  | GREATER =>
                       member right elem))


      fun foldl f x tree =
         (case tree of
             Leaf => x

           | Node (_, _, elem, left, right) =>
                foldl f (f (elem, foldl f x left)) right)


      fun foldr f x tree =
         (case tree of
             Leaf => x

           | Node (_, _, elem, left, right) =>
                foldr f (f (elem, foldr f x right)) left)


      fun toList tree = foldr (op ::) [] tree


      fun app f tree =
         (case tree of
             Leaf => ()
           | Node (_, _, elem, left, right) =>
                (
                app f left;
                f elem;
                app f right
                ))


      fun union tree1 tree2 =
         (case tree1 of
             Leaf => tree2

           | Node (_, _, key1, left1, right1) =>
                (case tree2 of
                    Leaf => tree1

                  | _ =>
                       let
                          val (_, left2, right2) =
                             split (fn key2 => Elem.compare (key1, key2)) tree2
                       in
                          join key1 (union left1 left2) (union right1 right2)
                       end))


      fun intersection tree1 tree2 =
         (case tree1 of
             Leaf => Leaf

           | Node (_, _, key1, left1, right1) =>
                (case tree2 of
                    Leaf => Leaf

                  | _ =>
                       let
                          val (labelo, left2, right2) =
                             split (fn key2 => Elem.compare (key1, key2)) tree2

                          val left = intersection left1 left2
                          val right = intersection right1 right2
                       in
                          (case labelo of
                              SOME _ =>
                                 join key1 left right

                            | NONE =>
                                 join' left right)
                       end))


      fun difference tree1 tree2 =
         (case tree1 of
             Leaf => Leaf

           | Node (_, _, key1, left1, right1) =>
                (case tree2 of
                    Leaf => tree1

                  | _ =>
                       let
                          val (labelo, left2, right2) =
                             split (fn key2 => Elem.compare (key1, key2)) tree2

                          val left = difference left1 left2
                          val right = difference right1 right2
                       in
                          (case labelo of
                              SOME _ =>
                                 join' left right

                            | NONE =>
                                 join key1 left right)
                       end))


      datatype q = Nil | E of elem * q | T of set * q

      fun eqMain qs1 qs2 =
         (case (qs1, qs2) of
             (Nil, Nil) => true
           | (Nil, E _) => false
           | (E _, Nil) => false

           | (T (Leaf, rest), _) => eqMain rest qs2

           | (_, T (Leaf, rest)) => eqMain qs1 rest

           | (T (Node (_, _, elem, left, right), rest), _) =>
                eqMain (T (left, E (elem, T (right, rest)))) qs2

           | (_, T (Node (_, _, elem, left, right), rest)) =>
                eqMain qs1 (T (left, E (elem, T (right, rest))))

           | (E (elem1, rest1), E (elem2, rest2)) =>
                Elem.eq (elem1, elem2) andalso eqMain rest1 rest2)

      fun eq (set1, set2) = eqMain (T (set1, Nil)) (T (set2, Nil))


      fun subsetMain qs1 qs2 =
         (case (qs1, qs2) of
             (Nil, _) => true
           | (E _, Nil) => false

           | (T (Leaf, rest), _) => subsetMain rest qs2

           | (_, T (Leaf, rest)) => subsetMain qs1 rest

           | (T (Node (_, _, elem, left, right), rest), _) =>
                subsetMain (T (left, E (elem, T (right, rest)))) qs2

           | (_, T (Node (_, _, elem, left, right), rest)) =>
                subsetMain qs1 (T (left, E (elem, T (right, rest))))

           | (E (elem1, rest1), E (elem2, rest2)) =>
                (case Elem.compare (elem1, elem2) of
                    LESS =>
                       false

                  | EQUAL =>
                       subsetMain rest1 rest2

                  | GREATER =>
                       subsetMain qs1 rest2))

      fun subset (set1, set2) = subsetMain (T (set1, Nil)) (T (set2, Nil))

      fun partitionlt tree key =
         let
            val (_, left, right) =
               split 
                  (fn key' => 
                      (case Elem.compare (key, key') of
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
                  (fn key' => 
                      (case Elem.compare (key, key') of
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

      exception Empty

      fun least tree =
         (case tree of
             Leaf => raise Empty

           | Node (_, _, x, Leaf, _) => x

           | Node (_, _, _, left, _) => least left)

      fun greatest tree =
         (case tree of
             Leaf => raise Empty

           | Node (_, _, x, _, Leaf) => x

           | Node (_, _, _, _, right) => greatest right)

      fun leastGt tree key =
         (case tree of
             Leaf => raise Empty

           | Node (_, _, key', left, right) =>
                (case Elem.compare (key, key') of
                    LESS =>
                       (leastGt left key
                        handle Empty => key')

                  | EQUAL => least right

                  | GREATER => leastGt right key))

      fun leastGeq tree key =
         (case tree of
             Leaf => raise Empty

           | Node (_, _, key', left, right) =>
                (case Elem.compare (key, key') of
                    LESS =>
                       (leastGeq left key
                        handle Empty => key')

                  | EQUAL => key'

                  | GREATER => leastGeq right key))

      fun greatestLt tree key =
         (case tree of
             Leaf => raise Empty

           | Node (_, _, key', left, right) =>
                (case Elem.compare (key, key') of
                    LESS => greatestLt left key

                  | EQUAL => greatest left

                  | GREATER => 
                       (greatestLt right key
                        handle Empty => key')))

      fun greatestLeq tree key =
         (case tree of
             Leaf => raise Empty

           | Node (_, _, key', left, right) =>
                (case Elem.compare (key, key') of
                    LESS => greatestLeq left key

                  | EQUAL => key'

                  | GREATER => 
                       (greatestLeq right key
                        handle Empty => key')))

   end

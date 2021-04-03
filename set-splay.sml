
functor SplaySet (structure Elem : ORDERED)
   :> SET where type elem = Elem.t
   =
   struct

      type elem = Elem.t

      open SplayTree

      type set = elem tree

      val empty = Leaf


      fun isEmpty tree =
         (case tree of
             Leaf => true
           | Node _ => false)


      fun singleton elem =
         Node (ref (1, elem, Leaf, Leaf))


      fun insert tree elem =
         (case tree of
             Leaf =>
                singleton elem

           | Node root =>
                let
                   val (order, (_, elem', left, right)) =
                      findAndSplay (fn elem' => Elem.compare (elem, elem')) root []
                in
                   (case order of
                       EQUAL => tree

                     | LESS =>
                           Node' (elem, left, Node' (elem', Leaf, right))

                     | GREATER =>
                           Node' (elem, Node' (elem', left, Leaf), right))
                end)


      fun remove tree elem =
         (case tree of
             Leaf =>
                empty

           | Node root =>
                let
                   val (order, (_, _, left, right)) =
                      findAndSplay (fn elem' => Elem.compare (elem, elem')) root []
                in
                   (case order of
                       EQUAL => join left right

                     | _ => tree)
                end)


      fun member tree elem =
         (case tree of
             Leaf => false

           | Node root =>
                (case findAndSplay (fn elem' => Elem.compare (elem, elem')) root [] of
                    (EQUAL, _) => true

                  | _ => false))


      fun union tree1 tree2 =
         (case tree1 of
             Leaf =>
                tree2

           | Node (ref (_, key1, left1, right1)) =>
                (case tree2 of
                    Leaf =>
                       tree1

                  | Node root2 =>
                       let
                          val (left2, _, right2) =
                             split (fn key2 => Elem.compare (key1, key2)) root2
                       in
                          Node' (key1, union left1 left2, union right1 right2)
                       end))


      fun intersection tree1 tree2 =
         (case tree1 of
             Leaf => 
                Leaf

           | Node (ref (_, key1, left1, right1)) =>
                (case tree2 of
                    Leaf =>
                       Leaf

                  | Node root2 =>
                       let
                          val (left2, middle, right2) =
                             split (fn key2 => Elem.compare (key1, key2)) root2

                          val left = intersection left1 left2
                          val right = intersection right1 right2
                       in
                          (case middle of
                              SOME _ => Node' (key1, left, right)

                            | NONE => join left right)
                       end))


      fun difference tree1 tree2 =
         (case tree1 of
             Leaf => 
                Leaf

           | Node (ref (_, key1, left1, right1)) =>
                (case tree2 of
                    Leaf =>
                       tree1

                  | Node root2 =>
                       let
                          val (left2, middle, right2) =
                             split (fn key2 => Elem.compare (key1, key2)) root2

                          val left = difference left1 left2
                          val right = difference right1 right2
                       in
                          (case middle of
                              SOME _ => join left right

                            | NONE => Node' (key1, left, right))
                       end))


      datatype q = Nil | E of elem * q | T of set * q

      fun eqMain qs1 qs2 =
         (case (qs1, qs2) of
             (Nil, Nil) => true
           | (Nil, E _) => false
           | (E _, Nil) => false

           | (T (Leaf, rest), _) => eqMain rest qs2

           | (_, T (Leaf, rest)) => eqMain qs1 rest

           | (T (Node (ref (_, elem, left, right)), rest), _) =>
                eqMain (T (left, E (elem, T (right, rest)))) qs2

           | (_, T (Node (ref (_, elem, left, right)), rest)) =>
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

           | (T (Node (ref (_, elem, left, right)), rest), _) =>
                subsetMain (T (left, E (elem, T (right, rest)))) qs2

           | (_, T (Node (ref (_, elem, left, right)), rest)) =>
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


      fun foldl f x tree =
         (case tree of
             Leaf => x
           | Node (ref (_, elem, left, right)) =>
                foldl f (f (elem, foldl f x left)) right)


      fun foldr f x tree =
         (case tree of
             Leaf => x
           | Node (ref (_, elem, left, right)) =>
                foldr f (f (elem, foldr f x right)) left)


      fun toList tree = foldr (op ::) [] tree

      fun app f tree =
         (case tree of
             Leaf => ()
           | Node (ref (_, elem, left, right)) =>
                (
                app f left;
                f elem;
                app f right
                ))

      fun partitionlt tree key =
         (case tree of
             Leaf => (Leaf, Leaf)

           | Node root =>
                let 
                   val (left, _, right) = 
                      split 
                         (fn key' => 
                             (case Elem.compare (key, key') of
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
                         (fn key' => 
                             (case Elem.compare (key, key') of
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


      exception Empty


      fun least tree =
         (case tree of
             Leaf =>
                raise Empty

           | Node root =>
                let
                   val (x, _) = splayMin root
                in
                   x
                end)


      fun greatest tree =
         (case tree of
             Leaf =>
                raise Empty

           | Node root =>
                let
                   val (x, _) = splayMax root
                in
                   x
                end)


      fun leastGt tree key =
         (case tree of
             Leaf =>
                raise Empty

           | Node root =>
                let
                   val (order, (_, x, left, right)) =
                      findAndSplay (fn key' => Elem.compare (key, key')) root []
                in
                   (case order of
                       GREATER => x
                     | _ => least right)
                end)


      fun leastGeq tree key =
         (case tree of
             Leaf =>
                raise Empty

           | Node root =>
                let
                   val (order, (_, x, left, right)) =
                      findAndSplay (fn key' => Elem.compare (key, key')) root []
                in
                   (case order of
                       LESS => least right
                     | _ => x)
                end)


      fun greatestLt tree key =
         (case tree of
             Leaf =>
                raise Empty

           | Node root =>
                let
                   val (order, (_, x, left, right)) =
                      findAndSplay (fn key' => Elem.compare (key, key')) root []
                in
                   (case order of
                       LESS => x
                     | _ => greatest left)
                end)


      fun greatestLeq tree key =
         (case tree of
             Leaf =>
                raise Empty

           | Node root =>
                let
                   val (order, (_, x, left, right)) =
                      findAndSplay (fn key' => Elem.compare (key, key')) root []
                in
                   (case order of
                       GREATER => greatest left
                     | _ => x)
                end)

   end

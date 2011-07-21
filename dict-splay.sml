
functor SplayDict (structure Key : ORDERED)
   :> DICT where type key = Key.t
   =
   struct

      type key = Key.t

      open SplayTree

      type 'a dict = int * (key * 'a) tree

      exception Absent

      val empty = (0, Leaf)

      fun singleton key datum =
         (1, Node (ref ((key, datum), Leaf, Leaf)))

      fun insert (n, tree) key datum =
         (case tree of
             Leaf =>
                singleton key datum
           | Node root =>
                let
                   val (order, node' as (label as (key', _), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          (n, Node (ref ((key, datum), left, right)))
                     | LESS =>
                          (n+1,
                           Node (ref ((key, datum), left, Node (ref (label, Leaf, right)))))
                     | GREATER =>
                          (n+1,
                           Node (ref ((key, datum), Node (ref (label, left, Leaf)), right))))
                end)

      fun operate (n, tree) key absentf presentf =
         (case tree of
             Leaf =>
                let
                   val datum = absentf ()
                in
                   (NONE, datum, singleton key datum)
                end
           | Node root =>
                let
                   val (order, (label as (key', datum'), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          let
                             val datum = presentf datum'
                          in
                             (SOME datum', datum,
                              (n, Node (ref ((key, datum), left, right))))
                          end
                     | LESS =>
                          let
                             val datum = absentf ()
                          in
                             (NONE, datum,
                              (n+1,
                               Node (ref ((key, datum), left, Node (ref (label, Leaf, right))))))
                          end
                     | GREATER =>
                          let
                             val datum = absentf ()
                          in
                             (NONE, datum,
                              (n+1,
                               Node (ref ((key, datum), Node (ref (label, left, Leaf)), right))))
                          end)
                end)

      fun insertMerge dict key x f =
         #3 (operate dict key (fn () => x) f)

      fun find (n, tree) key =
         (case tree of
             Leaf => NONE
           | Node root =>
                (case findAndSplay (fn (key', _) => Key.compare (key, key')) root [] of
                    (EQUAL, ((_, datum), _, _)) =>
                       SOME datum
                  | _ => NONE))

      fun lookup (n, tree) key =
         (case tree of
             Leaf => raise Absent
           | Node root =>
                (case findAndSplay (fn (key', _) => Key.compare (key, key')) root [] of
                    (EQUAL, ((_, datum), _, _)) =>
                       datum
                  | _ =>
                       raise Absent))

      fun unionMain tree1 tree2 f =
         (case tree1 of
             Leaf =>
                (SplayTree.size tree2, tree2)
           | Node (ref (label1 as (key1, datum1), left1, right1)) =>
                (case tree2 of
                    Leaf =>
                       (SplayTree.size tree1, tree1)
                  | Node root2 =>
                       let
                          val (order, (label2 as (key2, datum2), left2, right2)) =
                             findAndSplay (fn (key', _) => Key.compare (key1, key')) root2 []

                          val (left2', right2') =
                             (case order of
                                 EQUAL =>
                                    (left2, right2)
                               | LESS =>
                                    (left2, Node (ref (label2, Leaf, right2)))
                               | GREATER =>
                                    (Node (ref (label2, left2, Leaf)), right2))

                          val (nleft, left') = unionMain left1 left2' f
                          val (nright, right') = unionMain right1 right2' f
                       in
                          (case order of
                              EQUAL =>
                                 (1+nleft+nright,
                                  Node (ref ((key1, f (key1, datum1, datum2)), left', right')))
                            | _ =>
                                 (1+nleft+nright,
                                  Node (ref (label1, left', right'))))
                       end))

      fun union (dict1 as (n1, tree1)) (dict2 as (n2, tree2)) f =
         if n1 = 0 then
            dict2
         else if n2 = 0 then
            dict1
         else if n1 >= n2 then
            unionMain tree1 tree2 f
         else
            unionMain tree2 tree1 (fn (key, x, y) => f (key, y, x))

      fun isEmpty (n, _) = n = 0

      fun member (n, tree) key =
         (case tree of
             Leaf => false
           | Node root =>
                (case findAndSplay (fn (key', _) => Key.compare (key, key')) root [] of
                    (EQUAL, _) =>
                       true
                  | _ => false))

      fun size (n, _) = n

      fun foldlMain f x tree =
         (case tree of
             Leaf => x
           | Node (ref ((key, datum), left, right)) =>
                foldlMain f (f (key, datum, foldlMain f x left)) right)

      fun foldrMain f x tree =
         (case tree of
             Leaf => x
           | Node (ref ((key, datum), left, right)) =>
                foldrMain f (f (key, datum, foldrMain f x right)) left)

      fun toList (_, tree) =
         foldrMain (fn (key, datum, l) => (key, datum) :: l) [] tree

      fun domain (_, tree) =
         foldrMain (fn (key, _, l) => key :: l) [] tree

      fun foldl f x (_, tree) = foldlMain f x tree

      fun foldr f x (_, tree) = foldrMain f x tree

      fun mapMain f tree =
         (case tree of
             Leaf => Leaf
           | Node (ref ((key, datum), left, right)) =>
                Node (ref ((key, f datum), mapMain f left, mapMain f right)))

      fun map f (n, tree) = (n, mapMain f tree)

      fun appMain f tree =
         (case tree of
             Leaf => ()
           | Node (ref (label, left, right)) =>
                (
                appMain f left;
                f label;
                appMain f right
                ))

      fun app f (_, tree) = appMain f tree
         
   end

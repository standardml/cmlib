
functor SplayRDict (structure Key : ORDERED)
   :>
   RDICT where type key = Key.t
   =
   struct

      type key = Key.t

      open SplayTree

      type 'a dict = (key * 'a) tree

      exception Absent

      val empty = Leaf

      fun singleton key datum =
         Node (ref ((key, datum), Leaf, Leaf))

      fun insert tree key datum =
         (case tree of
             Leaf =>
                singleton key datum
           | Node root =>
                let
                   val (order, node' as (label, left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          Node (ref ((key, datum), left, right))
                     | LESS =>
                          Node (ref ((key, datum), left, Node (ref (label, Leaf, right))))
                     | GREATER =>
                          Node (ref ((key, datum), Node (ref (label, left, Leaf)), right)))
                end)

      fun insert' tree key datum =
         (case tree of
             Leaf =>
                (singleton key datum, false)
           | Node root =>
                let
                   val (order, node' as (label, left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          (Node (ref ((key, datum), left, right)), true)
                     | LESS =>
                          (Node (ref ((key, datum), left, Node (ref (label, Leaf, right)))), false)
                     | GREATER =>
                          (Node (ref ((key, datum), Node (ref (label, left, Leaf)), right)), false))
                end)

      fun remove tree key =
         (case tree of
             Leaf => empty
           | Node root =>
                let
                   val (order, (_, left, right)) =
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
                   val (order, (_, left, right)) =
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
                   val (order, (label as (key', datum'), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          (case presentf datum' of
                              NONE =>
                                 (SOME datum', NONE, join left right)
                            | y as SOME datum =>
                                 (SOME datum', y,
                                  Node (ref ((key, datum), left, right))))
                     | LESS =>
                          (case absentf () of
                              NONE =>
                                 (NONE, NONE, Node (ref (label, left, right)))
                            | y as SOME datum =>
                                 (NONE, y,
                                  Node (ref ((key, datum), left, Node (ref (label, Leaf, right))))))
                     | GREATER =>
                          (case absentf () of
                              NONE =>
                                 (NONE, NONE, Node (ref (label, left, right)))
                            | y as SOME datum =>
                                 (NONE, y,
                                  Node (ref ((key, datum), Node (ref (label, left, Leaf)), right)))))
                end)

      fun operate dict key absentf presentf =
         let
            val (x, y, d) = operate' dict key (SOME o absentf) (SOME o presentf)
         in
            (x, valOf y, d)
         end
         
      fun insertMerge dict key x f =
         #3 (operate' dict key (fn () => SOME x) (SOME o f))

      fun find tree key =
         (case tree of
             Leaf => NONE
           | Node root =>
                (case findAndSplay (fn (key', _) => Key.compare (key, key')) root [] of
                    (EQUAL, ((_, datum), _, _)) =>
                       SOME datum
                  | _ => NONE))

      fun lookup tree key =
         (case tree of
             Leaf => raise Absent
           | Node root =>
                (case findAndSplay (fn (key', _) => Key.compare (key, key')) root [] of
                    (EQUAL, ((_, datum), _, _)) =>
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
                    (EQUAL, _) =>
                       true
                  | _ => false))

      fun foldl f x tree =
         (case tree of
             Leaf => x
           | Node (ref ((key, datum), left, right)) =>
                foldl f (f (key, datum, foldl f x left)) right)

      fun foldr f x tree =
         (case tree of
             Leaf => x
           | Node (ref ((key, datum), left, right)) =>
                foldr f (f (key, datum, foldr f x right)) left)

      fun toList tree =
         foldr (fn (key, datum, l) => (key, datum) :: l) [] tree

      fun domain tree =
         foldr (fn (key, _, l) => key :: l) [] tree

      fun map f tree =
         (case tree of
             Leaf => Leaf
           | Node (ref ((key, datum), left, right)) =>
                Node (ref ((key, f datum), map f left, map f right)))

      fun app f tree =
         (case tree of
             Leaf => ()
           | Node (ref (label, left, right)) =>
                (
                app f left;
                f label;
                app f right
                ))



      fun partition tree key =
         (case tree of
             Leaf => (empty, NONE, empty)
           | Node root =>
                let
                   val (order, (label as (_, datum), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          (left, SOME datum, right)
                     | LESS =>
                          (left, NONE, Node (ref (label, Leaf, right)))
                     | GREATER =>
                          (Node (ref (label, left, Leaf)), NONE, right))
                end)

      fun partitionlt tree key =
         (case tree of
             Leaf => (empty, empty)
           | Node root =>
                let
                   val (order, (label, left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          (left, Node (ref (label, Leaf, right)))
                     | LESS =>
                          (left, Node (ref (label, Leaf, right)))
                     | GREATER =>
                          (Node (ref (label, left, Leaf)), right))
                end)

      fun partitiongt tree key =
         (case tree of
             Leaf => (empty, empty)
           | Node root =>
                let
                   val (order, (label, left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       EQUAL =>
                          (Node (ref (label, left, Leaf)), right)
                     | LESS =>
                          (left, Node (ref (label, Leaf, right)))
                     | GREATER =>
                          (Node (ref (label, left, Leaf)), right))
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
                   val (order, ((_, datum), left, right)) =
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
                   val (order, ((_, datum), left, right)) =
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
                   val (order, ((_, datum), left, right)) =
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
                   val (order, ((_, datum), left, right)) =
                      findAndSplay (fn (key', _) => Key.compare (key, key')) root []
                in
                   (case order of
                       GREATER => greatest left
                     | _ => datum)
                end)

   end


functor SplayDict (structure Key : ORDERED)
   :>
   DICT where type key = Key.t
   =
   DictFun (SplayRDict (structure Key = Key))

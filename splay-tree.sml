
structure SplayTree =
   struct

      datatype 'a tree =
         Leaf
       | Node of 'a node ref

      (* int = size *)
      withtype 'a node = int * 'a * 'a tree * 'a tree

      (* NB: We assign to a reference only with a rearranged version of the same tree.
         Thus, all assignments are benign (that is, no assignment changes the semantics
         of the tree).  Regrouping always requires a new reference be allocated.
      *)

      datatype 'a zipelem =
         LEFT of 'a node ref * 'a * 'a tree
       | RIGHT of 'a node ref * 'a * 'a tree


      fun size tree =
         (case tree of
             Leaf => 0
           | Node (ref (sz, _, _, _)) => sz)


      fun mknode (label, left, right) = 
         (size left + size right + 1, label, left, right)


      fun mknoder (label, left, right) =
         Node (ref (size left + size right + 1, label, left, right))


      fun splay (node as (_, label, left, right)) zipper =
         (case zipper of
             [] =>
                node

           | [LEFT (parent, label1, right1)] =>
                (* zig case *)
                let
                   val right' = 
                      mknoder (label1, right, right1)

                   val node' = mknode (label, left, right')
                in
                   parent := node';
                   node'
                end

           | [RIGHT (parent, label1, left1)] =>
                (* zag case *)
                let
                   val left' =
                      mknoder (label1, left1, left)
                      
                   val node' = mknode (label, left', right)
                in
                   parent := node';
                   node'
                end

           | LEFT (_, label1, right1) :: LEFT (grandparent, label2, right2) :: rest =>
                (* zig-zig case *)
                let
                   val right' =
                      mknoder (label1, right, mknoder (label2, right1, right2))

                   val node' = mknode (label, left, right')
                in
                   grandparent := node';
                   splay node' rest
                end

           | RIGHT (_, label1, left1) :: RIGHT (grandparent, label2, left2) :: rest =>
                (* zag-zag case *)
                let
                   val left' =
                      mknoder (label1, mknoder (label2, left2, left1), left)

                   val node' = mknode (label, left', right)
                in
                   grandparent := node';
                   splay node' rest
                end

           | RIGHT (_, label1, left1) :: LEFT (grandparent, label2, right2) :: rest =>
                (* zig-zag case *)
                let
                   val left' = mknoder (label1, left1, left)

                   val right' = mknoder (label2, right, right2)

                   val node' = mknode (label, left', right')
                in
                   grandparent := node';
                   splay node' rest
                end

           | LEFT (_, label1, right1) :: RIGHT (grandparent, label2, left2) :: rest =>
                (* zag-zig case *)
                let
                   val left' = mknoder (label2, left2, left)

                   val right' = mknoder (label1, right, right1)
                   
                   val node' = mknode (label, left', right')
                in
                   grandparent := node';
                   splay node' rest
                end)


      fun findAndSplay f (root as ref (node as (_, label, left, right))) zipper =
         (case f label of
             EQUAL =>
                (EQUAL, splay node zipper)

           | LESS =>
                (case left of
                    Leaf =>
                       (LESS, splay node zipper)

                  | Node root' =>
                       findAndSplay f root' (LEFT (root, label, right) :: zipper))

           | GREATER =>
                (case right of
                    Leaf =>
                       (GREATER, splay node zipper)

                  | Node root' =>
                       findAndSplay f root' (RIGHT (root, label, left) :: zipper)))


      fun split f root =
         let
            val (order, (_, label, left, right)) =
               findAndSplay f root []
         in
            (case order of
                EQUAL =>
                   (left, SOME label, right)

              | LESS =>
                   (left, NONE, mknoder (label, Leaf, right))

              | GREATER =>
                   (mknoder (label, left, Leaf), NONE, right))
         end


      fun splayMin root =
         let
            val (_, (_, label, _, tree)) =
               findAndSplay (fn _ => LESS) root []
               (* must return (LESS, (label, Leaf, tree)) *)
         in
            (label, tree)
         end


      fun splayMax root =
         let
            val (_, (_, label, tree, _)) =
               findAndSplay (fn _ => GREATER) root []
               (* must return (GREATER, (label, tree, Leaf)) *)
         in
            (label, tree)
         end


      fun join left right =
         (case right of
             Leaf =>
                left

           | Node root =>
                let
                   val (label, right') = splayMin root
                in
                   mknoder (label, left, right')
                end)

   end

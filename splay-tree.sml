
structure SplayTree =
   struct

      datatype 'a tree =
         Leaf
       | Node of 'a node ref

      withtype 'a node = 'a * 'a tree * 'a tree

      (* NB: We assign to a reference only with a rearranged version of the same tree.
         Thus, all assignments are benign (that is, no assignment changes the semantics
         of the tree).  Regrouping always requires a new reference be allocated.
      *)


      datatype 'a zipelem =
         LEFT of 'a node ref * 'a * 'a tree
       | RIGHT of 'a node ref * 'a * 'a tree

      fun splay (node as (label, left, right)) zipper =
         (case zipper of
             [] =>
                node
           | [LEFT (parent, label1, right1)] =>
                (* zig case *)
                let
                   val right' =
                      Node (ref (label1, right, right1))

                   val node' = (label, left, right')
                in
                   parent := node';
                   node'
                end
           | [RIGHT (parent, label1, left1)] =>
                (* zag case *)
                let
                   val left' =
                      Node (ref (label1, left1, left))
                      
                   val node' = (label, left', right)
                in
                   parent := node';
                   node'
                end
           | LEFT (_, label1, right1) :: LEFT (grandparent, label2, right2) :: rest =>
                (* zig-zig case *)
                let
                   val right' =
                      Node (ref (label1, right, Node (ref (label2, right1, right2))))

                   val node' = (label, left, right')
                in
                   grandparent := node';
                   splay node' rest
                end
           | RIGHT (_, label1, left1) :: RIGHT (grandparent, label2, left2) :: rest =>
                (* zag-zag case *)
                let
                   val left' =
                      Node (ref (label1, Node (ref (label2, left2, left1)), left))

                   val node' = (label, left', right)
                in
                   grandparent := node';
                   splay node' rest
                end
           | RIGHT (_, label1, left1) :: LEFT (grandparent, label2, right2) :: rest =>
                (* zig-zag case *)
                let
                   val left' = Node (ref (label1, left1, left))

                   val right' = Node (ref (label2, right, right2))

                   val node' = (label, left', right')
                in
                   grandparent := node';
                   splay node' rest
                end
           | LEFT (_, label1, right1) :: RIGHT (grandparent, label2, left2) :: rest =>
                (* zag-zig case *)
                let
                   val left' = Node (ref (label2, left2, left))

                   val right' = Node (ref (label1, right, right1))
                   
                   val node' = (label, left', right')
                in
                   grandparent := node';
                   splay node' rest
                end)


      fun findAndSplay f (root as ref (node as (label, left, right))) zipper =
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
            val (order, (label, left, right)) =
               findAndSplay f root []
               
            val (left', right') =
               (case order of
                   EQUAL =>
                      (left, right)
                 | LESS =>
                      (left, Node (ref (label, Leaf, right)))
                 | GREATER =>
                      (Node (ref (label, left, Leaf)), right))
         in
            (order, left', right')
         end

      fun splayMin root =
         let
            val (_, (label, _, tree)) =
               findAndSplay (fn _ => LESS) root []
               (* must return (LESS, (label, Leaf, tree)) *)
         in
            (label, tree)
         end

      fun splayMax root =
         let
            val (_, (label, tree, _)) =
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
                   Node (ref (label, left, right'))
                end)

      fun size tree =
         (case tree of
             Leaf => 0
           | Node (ref (_, left, right)) => 1 + size left + size right)

   end

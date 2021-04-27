
structure RedBlackTree =
   struct

      datatype color = RED | BLACK

      (* invariant: int = size *)
      datatype 'a tree =
         Leaf
       | Node of color * int * 'a * 'a tree * 'a tree

      (* Red-black invariant: no red node has a red child

         Black height invariant: for every t = Node(_, _, left, right),
         black-height(left) = black-height(right)

         Leaves are considered black, but do not contribute to black height.
      *)

      datatype 'a zipelem =
         LEFT of color * 'a * 'a tree
       | RIGHT of color * 'a * 'a tree


      fun size tree =
         (case tree of
             Leaf => 0
           | Node (_, sz, _, _, _) => sz)

      (* Hopefully this gets inlined.  Doing all the size computations in one place
         makes me much more confident they are done correctly.
      *)
      fun mknode (color, label, left, right) =
         Node (color, size left + size right + 1, label, left, right)

         
      fun zip tree zipper =
         (case zipper of
             [] => tree

           | LEFT (color, label, right) :: rest =>
                zip (mknode (color, label, tree, right)) rest

           | RIGHT (color, label, left) :: rest =>
                zip (mknode (color, label, left, tree)) rest)


      (* Precondition:
         (zip (Node (RED, bh, label, left, right) bh zipper) satisfies the black-height
         invariant, and also satisfies the red-black invariant except possibly locally
      *)
      fun zipRed (label, left, right) zipper =
         (case zipper of
             [] =>
                mknode (BLACK, label, left, right)

           | LEFT (BLACK, label1, right1) :: rest =>
                zip
                   (mknode (BLACK, label1,
                          mknode (RED, label, left, right),
                          right1))
                   rest

           | RIGHT (BLACK, label1, left1) :: rest =>
                zip
                   (mknode (BLACK, label1,
                          left1,
                          mknode (RED, label, left, right)))
                   rest

           | LEFT (RED, label1, right1) ::
             LEFT (_ (* BLACK *), label2, Node (RED, sz3, label3, left3, right3)) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zipRed
                   (label2,
                    mknode (BLACK, label1,
                          mknode (RED, label, left, right),
                          right1),
                    Node (BLACK, sz3, label3, left3, right3))
                   rest

           | LEFT (RED, label1, right1) ::
             RIGHT (_ (* BLACK *), label2, Node (RED, sz3, label3, left3, right3)) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zipRed
                   (label2,
                    Node (BLACK, sz3, label3, left3, right3),
                    mknode (BLACK, label1,
                          mknode (RED, label, left, right),
                          right1))
                   rest

           | RIGHT (RED, label1, left1) ::
             LEFT (_ (* BLACK *), label2, Node (RED, sz3, label3, left3, right3)) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zipRed
                   (label2,
                    mknode (BLACK, label1,
                          left1,
                          mknode (RED, label, left, right)),
                    Node (BLACK, sz3, label3, left3, right3))
                   rest

           | RIGHT (RED, label1, left1) ::
             RIGHT (_ (* BLACK *), label2, Node (RED, sz3, label3, left3, right3)) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zipRed
                   (label2,
                    Node (BLACK, sz3, label3, left3, right3),
                    mknode (BLACK, label1,
                          left1,
                          mknode (RED, label, left, right)))
                   rest

           | LEFT (RED, label1, right1) ::
             LEFT (_ (* BLACK *), label2, node3) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zip
                   (mknode (BLACK, label1,
                          mknode (RED, label, left, right),
                          mknode (RED, label2, right1, node3)))
                   rest

           | LEFT (RED, label1, right1) ::
             RIGHT (_ (* BLACK *), label2, node3) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zip
                   (mknode (BLACK, label,
                          mknode (RED, label2, node3, left),
                          mknode (RED, label1, right, right1)))
                   rest

           | RIGHT (RED, label1, left1) ::
             LEFT (_ (* BLACK *), label2, node3) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zip
                   (mknode (BLACK, label,
                          mknode (RED, label1, left1, left),
                          mknode (RED, label2, right, node3)))
                   rest

           | RIGHT (RED, label1, left1) ::
             RIGHT (_ (* BLACK *), label2, node3) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zip
                   (mknode (BLACK, label1, 
                          mknode (RED, label2, node3, left1),
                          mknode (RED, label, left, right)))
                   rest

           | [LEFT (RED, label1, right1)] =>
                mknode (BLACK, label1,
                      mknode (RED, label, left, right),
                      right1)
   
           | [RIGHT (RED, label1, left1)] =>
                mknode (BLACK, label1,
                      left1,
                      mknode (RED, label, left, right)))


      (* Precondition:
         1. tree is black
         2. (zip tree zipper) satisfies the red-black invariant, and it
            would satisfy the black-height invariant if the black-height of
            tree were one higher
      *)
      fun zipBlack tree zipper =
         (case zipper of
             [] => tree


           | LEFT (color1, label1, Node (_ (* BLACK *), _, label2,
                                         left2,
                                         Node (RED, sz3, label3, left3, right3))) :: rest =>
                zip
                   (mknode (color1, label2,
                          mknode (BLACK, label1, tree, left2),
                          Node (BLACK, sz3, label3, left3, right3)))
                   rest

           | RIGHT (color1, label1, Node (_ (* BLACK *), _, label2,
                                          Node (RED, sz3, label3, left3, right3),
                                          right2)) :: rest =>
                (* Sibling is BLACK by red-black invariant. *)
                zip
                   (mknode (color1, label2,
                          Node (BLACK, sz3, label3, left3, right3),
                          mknode (BLACK, label1, right2, tree)))
                   rest



           | LEFT (color1, label1, Node (_ (* BLACK *), _, label2,
                                         Node (RED, _, label3, left3, right3),
                                         right2)) :: rest =>
                (* Sibling is BLACK by red-black invariant. *)
                zip
                   (mknode (color1, label3,
                          mknode (BLACK, label1, tree, left3),
                          mknode (BLACK, label2, right3, right2)))
                   rest

           | RIGHT (color1, label1, Node (_ (* BLACK *), _, label2,
                                         left2,
                                         Node (RED, _, label3, left3, right3))) :: rest =>
                (* Sibling is BLACK by red-black invariant. *)
                zip
                   (mknode (color1, label3,
                          mknode (BLACK, label2, left2, left3),
                          mknode (BLACK, label1, right3, tree)))
                   rest



           | LEFT (RED, label1, Node (_ (* BLACK *), sz2, label2, left2, right2)) :: rest =>
                (* Sibling is BLACK by red-black invariant.
                   Previous cases rule out left2 or right2 being a red node.
                *)
                zip
                   (mknode (BLACK, label1,
                          tree,
                          Node (RED, sz2, label2, left2, right2)))
                   rest

           | RIGHT (RED, label1, Node (_ (* BLACK *), sz2, label2, left2, right2)) :: rest =>
                (* Sibling is BLACK by red-black invariant.
                   Previous cases rule out left2 or right2 being a red node.
                *)
                zip
                   (mknode (BLACK, label1,
                          Node (RED, sz2, label2, left2, right2),
                          tree))
                   rest



           | LEFT (BLACK, label1, Node (BLACK, sz2, label2, left2, right2)) :: rest =>
                (* Previous cases rule out left2 or right2 being a red node. *)
                zipBlack
                   (mknode (BLACK, label1,
                          tree,
                          Node (RED, sz2, label2, left2, right2)))
                   rest

           | RIGHT (BLACK, label1, Node (BLACK, sz2, label2, left2, right2)) :: rest =>
                (* Previous cases rule out left2 or right2 being a red node. *)
                zipBlack
                   (mknode (BLACK, label1,
                          Node (RED, sz2, label2, left2, right2),
                          tree))
                   rest



           | LEFT (BLACK, label1, Node (RED, _, label2, left2, right2)) :: rest =>
                zipBlack
                   tree
                   (LEFT (RED, label1, left2) :: LEFT (BLACK, label2, right2) :: rest)

           | RIGHT (BLACK, label1, Node (RED, _, label2, left2, right2)) :: rest =>
                zipBlack
                   tree
                   (RIGHT (RED, label1, right2) :: RIGHT (BLACK, label2, left2) :: rest)



           | LEFT (_, _, Leaf) :: _ =>
                (* Impossible by black-height invariant. *)
                raise (Fail "invariant")

           | RIGHT (_, _, Leaf) :: _ =>
                (* Impossible by black-height invariant. *)
                raise (Fail "invariant"))


      fun search f tree zipper =
         (case tree of
             Leaf =>
                (Leaf, zipper)

           | Node (color, _, label, left, right) =>
                (case f label of
                    LESS =>
                       search f left (LEFT (color, label, right) :: zipper)

                  | GREATER =>
                       search f right (RIGHT (color, label, left) :: zipper)

                  | EQUAL =>
                       (tree, zipper)))


      fun searchMin tree zipper =
         (case tree of
             Leaf => zipper

           | Node (color, _, label, left, right) =>
                searchMin left (LEFT (color, label, right) :: zipper))


      fun searchMax tree zipper =
         (case tree of
             Leaf => zipper

           | Node (color, _, label, left, right) =>
                searchMax right (RIGHT (color, label, left) :: zipper))


      (* Precondition:
         (zip (Node (color, _, _, Leaf, child)) zipper) is a valid tree,
         or (zip (Node (color, _, _, child, Leaf)) zipper) is a valid tree.
      *)
      fun deleteNearLeaf color child zipper =
         (case color of
             RED =>
                (* child cannot be RED, by red-black invariant,
                   so it must be Leaf, by black-height invariant.
                *)
                zip Leaf zipper

           | BLACK =>
                (case child of
                    Node (_ (* RED *), _, label, _ (* Leaf *), _ (* Leaf *)) =>
                       (* Must be RED with Leaf children, by black-height invariant. *)
                       zip (mknode (BLACK, label, Leaf, Leaf)) zipper

                  | Leaf =>
                       zipBlack Leaf zipper))

      (* Precondition:
         zip (Node (color, _, _, left, right)) zipper is a valid tree.
      *)
      fun delete color left right zipper =
         (case right of
             Leaf =>
                (case left of
                    Leaf =>
                       (case color of
                           RED =>
                              zip Leaf zipper

                         | BLACK =>
                              zipBlack Leaf zipper)
                  | _ =>
                       (case searchMax left [] of
                           RIGHT (colorLeftMin, labelLeftMin, leftLeftMin) :: zipper' =>
                              deleteNearLeaf
                                 colorLeftMin leftLeftMin
                                 (zipper' @ LEFT (color, labelLeftMin, right) :: zipper)

                         | _ =>
                              raise (Fail "postcondition")))
           | _ =>
                (case searchMin right [] of
                    LEFT (colorRightMin, labelRightMin, rightRightMin) :: zipper' =>
                       deleteNearLeaf
                          colorRightMin rightRightMin
                          (zipper' @ RIGHT (color, labelRightMin, left) :: zipper)

                  | _ =>
                       raise (Fail "postcondition")))


      fun blacken tree =
         (case tree of
             Node (RED, sz, label, left, right) =>
                Node (BLACK, sz, label, left, right)

           | _ => tree)


      fun blackHeight tree acc =
         (case tree of
             Leaf => acc

           | Node (RED, _, _, left, _) => blackHeight left acc

           | Node (BLACK, _, _, left, _) => blackHeight left (acc+1))


      (* precondition: blackHeight(tree) >= target >= 0

         find a black subtree along the left/right spine whose black-height is
         blackHeight(tree) - target.
      *)
      fun searchHeight leftward target tree zipper =
         (case tree of
             Leaf =>
                (Leaf, zipper)

           | Node (RED, _, label, left, right) =>
                if leftward then
                   searchHeight leftward target left
                   (LEFT (RED, label, right) :: zipper)
                else
                   searchHeight leftward target right
                   (RIGHT (RED, label, left) :: zipper)

           | Node (BLACK, _, label, left, right) =>
                if target = 0 then
                   (tree, zipper)
                else
                   if leftward then
                      searchHeight leftward (target-1) left
                      (LEFT (BLACK, label, right) :: zipper)
                   else
                      searchHeight leftward (target-1) right
                      (RIGHT (BLACK, label, left) :: zipper))


      fun join label left right =
         let
            (* without loss of generality, assume left and right have black roots *)
            val left = blacken left
            val right = blacken right
            
            (* This takes two O(log n) searches, which doesn't affect the asymptotic
               time.  There are ways we could avoid this, but none of them seem
               preferable.  For example, we could keep track of the black height, but
               that would involve doing that arithmetic everywhere, even in applications
               that never need a join.
            *)
            val lbh = blackHeight left 0
            val rbh = blackHeight right 0
         in
            (case Int.compare (lbh, rbh) of
                EQUAL =>
                   (* It's nice when this happens. *)
                   mknode (BLACK, label, left, right)
   
              | GREATER =>
                   let
                      val (left', zipper) = searchHeight false (lbh-rbh) left []
                   in
                      (* left' and right are both black and both have black height rbh *)
                      zipRed (label, left', right) zipper
                   end

              | LESS =>
                   let
                      val (right', zipper) = searchHeight true (rbh-lbh) right []
                   in
                      (* left and right' are both black and both have black height lbh *)
                      zipRed (label, left, right') zipper
                   end)
         end


      fun join' left right =
        (case left of
            Leaf => right

          | _ =>
               (case right of
                   Leaf => left

                 | _ =>
                      (case searchMax left [] of
                          RIGHT (color, label, leftsmall) :: zipper =>
                             join label (deleteNearLeaf color leftsmall zipper) right

                        | _ =>
                             raise (Fail "postcondition"))))


      fun split f tree =
         (case tree of
             Leaf =>
                (NONE, Leaf, Leaf)

           | Node (_, _, label, left, right) =>
                (case f label of
                    EQUAL =>
                       (SOME label, left, right)

                  | LESS =>
                       let
                          val (label', left', right') = split f left
                       in
                          (label', left', join label right' right)
                       end

                  | GREATER =>
                       let
                          val (label', left', right') = split f right
                       in
                          (label', join label left left', right')
                       end))

   end

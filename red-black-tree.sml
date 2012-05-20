
structure RedBlackTree =
   struct

      datatype color = RED | BLACK

      datatype 'a tree =
         Leaf
       | Node of color * 'a * 'a tree * 'a tree

      datatype 'a zipelem =
         LEFT of color * 'a * 'a tree
       | RIGHT of color * 'a * 'a tree
         
      fun zip tree zipper =
         (case zipper of
             [] => tree
           | LEFT (color, label, right) :: rest =>
                zip (Node (color, label, tree, right)) rest
           | RIGHT (color, label, left) :: rest =>
                zip (Node (color, label, left, tree)) rest)


      (* Precondition:
         (zip (Node (RED, label, left, right) zipper) satisfies the black-height
         invariant, but possibly not the red-black invariant.
      *)
      fun zipRed (label, left, right) zipper =
         (case zipper of
             [] =>
                Node (BLACK, label, left, right)

           | LEFT (BLACK, label1, right1) :: rest =>
                zip
                   (Node (BLACK, label1,
                          Node (RED, label, left, right),
                          right1))
                   rest

           | RIGHT (BLACK, label1, left1) :: rest =>
                zip
                   (Node (BLACK, label1,
                          left1,
                          Node (RED, label, left, right)))
                   rest

           | LEFT (RED, label1, right1) ::
             LEFT (_ (* BLACK *), label2, Node (RED, label3, left3, right3)) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zipRed
                   (label2,
                    Node (BLACK, label1,
                          Node (RED, label, left, right),
                          right1),
                    Node (BLACK, label3, left3, right3))
                   rest

           | LEFT (RED, label1, right1) ::
             RIGHT (_ (* BLACK *), label2, Node (RED, label3, left3, right3)) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zipRed
                   (label2,
                    Node (BLACK, label3, left3, right3),
                    Node (BLACK, label1,
                          Node (RED, label, left, right),
                          right1))
                   rest

           | RIGHT (RED, label1, left1) ::
             LEFT (_ (* BLACK *), label2, Node (RED, label3, left3, right3)) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zipRed
                   (label2,
                    Node (BLACK, label1,
                          left1,
                          Node (RED, label, left, right)),
                    Node (BLACK, label3, left3, right3))
                   rest

           | RIGHT (RED, label1, left1) ::
             RIGHT (_ (* BLACK *), label2, Node (RED, label3, left3, right3)) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zipRed
                   (label2,
                    Node (BLACK, label3, left3, right3),
                    Node (BLACK, label1,
                          left1,
                          Node (RED, label, left, right)))
                   rest

           | LEFT (RED, label1, right1) ::
             LEFT (_ (* BLACK *), label2, node3) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zip
                   (Node (BLACK, label1,
                          Node (RED, label, left, right),
                          Node (RED, label2, right1, node3)))
                   rest

           | LEFT (RED, label1, right1) ::
             RIGHT (_ (* BLACK *), label2, node3) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zip
                   (Node (BLACK, label,
                          Node (RED, label2, node3, left),
                          Node (RED, label1, right, right1)))
                   rest

           | RIGHT (RED, label1, left1) ::
             LEFT (_ (* BLACK *), label2, node3) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zip
                   (Node (BLACK, label,
                          Node (RED, label1, left1, left),
                          Node (RED, label2, right, node3)))
                   rest

           | RIGHT (RED, label1, left1) ::
             RIGHT (_ (* BLACK *), label2, node3) :: rest =>
                (* Grandparent is BLACK, by red-black invariant. *)
                zip
                   (Node (BLACK, label1, 
                          Node (RED, label2, node3, left1),
                          Node (RED, label, left, right)))
                   rest

           | [LEFT (RED, _, _)] =>
                (* The root cannot be red. *)
                raise (Fail "invariant")

           | [RIGHT (RED, _, _)] =>
                (* The root cannot be red. *)
                raise (Fail "invariant"))


      (* Precondition:
         1. tree is Leaf or BLACK
         2. (zip tree zipper) satisfies the red-black invariant, but the black-height
            of tree is 1 too small, unless zipper=[],
      *)
      fun zipBlack tree zipper =
         (case zipper of
             [] => tree


           | LEFT (color1, label1, Node (_ (* BLACK *), label2,
                                         left2,
                                         Node (RED, label3, left3, right3))) :: rest =>
                zip
                   (Node (color1, label2,
                          Node (BLACK, label1, tree, left2),
                          Node (BLACK, label3, left3, right3)))
                   rest

           | RIGHT (color1, label1, Node (_ (* BLACK *), label2,
                                          Node (RED, label3, left3, right3),
                                          right2)) :: rest =>
                (* Sibling is BLACK by red-black invariant. *)
                zip
                   (Node (color1, label2,
                          Node (BLACK, label3, left3, right3),
                          Node (BLACK, label1, right2, tree)))
                   rest



           | LEFT (color1, label1, Node (_ (* BLACK *), label2,
                                         Node (RED, label3, left3, right3),
                                         right2)) :: rest =>
                (* Sibling is BLACK by red-black invariant. *)
                zip
                   (Node (color1, label3,
                          Node (BLACK, label1, tree, left3),
                          Node (BLACK, label2, right3, right2)))
                   rest

           | RIGHT (color1, label1, Node (_ (* BLACK *), label2,
                                          left2,
                                          Node (RED, label3, left3, right3))) :: rest =>
                (* Sibling is BLACK by red-black invariant. *)
                zip
                   (Node (color1, label3,
                          Node (BLACK, label2, left2, left3),
                          Node (BLACK, label1, right3, tree)))
                   rest



           | LEFT (RED, label1, Node (_ (* BLACK *), label2, left2, right2)) :: rest =>
                (* Sibling is BLACK by red-black invariant.
                   Previous cases rule out left2 or right2 being a red node.
                *)
                zip
                   (Node (BLACK, label1,
                          tree,
                          Node (RED, label2, left2, right2)))
                   rest

           | RIGHT (RED, label1, Node (_ (* BLACK *), label2, left2, right2)) :: rest =>
                (* Sibling is BLACK by red-black invariant.
                   Previous cases rule out left2 or right2 being a red node.
                *)
                zip
                   (Node (BLACK, label1,
                          Node (RED, label2, left2, right2),
                          tree))
                   rest



           | LEFT (BLACK, label1, Node (BLACK, label2, left2, right2)) :: rest =>
                (* Previous cases rule out left2 or right2 being a red node. *)
                zipBlack
                   (Node (BLACK, label1,
                          tree,
                          Node (RED, label2, left2, right2)))
                   rest

           | RIGHT (BLACK, label1, Node (BLACK, label2, left2, right2)) :: rest =>
                (* Previous cases rule out left2 or right2 being a red node. *)
                zipBlack
                   (Node (BLACK, label1,
                          Node (RED, label2, left2, right2),
                          tree))
                   rest



           | LEFT (BLACK, label1, Node (RED, label2, left2, right2)) :: rest =>
                zipBlack
                   tree
                   (LEFT (RED, label1, left2) :: LEFT (BLACK, label2, right2) :: rest)

           | RIGHT (BLACK, label1, Node (RED, label2, left2, right2)) :: rest =>
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
           | Node (color, label, left, right) =>
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
           | Node (color, label, left, right) =>
                searchMin left (LEFT (color, label, right) :: zipper))

      fun searchMax tree zipper =
         (case tree of
             Leaf => zipper
           | Node (color, label, left, right) =>
                searchMax right (RIGHT (color, label, left) :: zipper))

      (* Precondition:
         (zip (Node (color, _, Leaf, child)) zipper) is a valid tree,
         or (zip (Node (color, _, child, Leaf)) zipper) is a valid tree.
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
                    Node (_ (* RED *), label, _ (* Leaf *), _ (* Leaf *)) =>
                       (* Must be RED with Leaf children, by black-height invariant. *)
                       zip (Node (BLACK, label, Leaf, Leaf)) zipper
                  | Leaf =>
                       zipBlack Leaf zipper))

      (* Precondition:
         zip (Node (color, _, left, right)) zipper is a valid tree.
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

   end

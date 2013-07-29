
(* Stable Quicksort.  Performs very badly on sorted input, due to poor pivot selection.
   Should perform well on random input.
*)

structure Quicksort :> SORT =
   struct

      (* returns buckets in reverse order *)
      fun split f pivot l acclt acceq accgt =
         (case l of
             [] => (acclt, acceq, accgt)
           | h :: t =>
                (case f (h, pivot) of
                    LESS =>
                       split f pivot t (h :: acclt) acceq accgt
                  | EQUAL =>
                       split f pivot t acclt (h :: acceq) accgt
                  | GREATER =>
                       split f pivot t acclt acceq (h :: accgt)))

      fun revOnto l acc =
         (case l of
             [] => acc
           | h :: t =>
                revOnto t (h :: acc))

      (* stable *)
      fun sortOntoFwd f l acc =
         (case l of
             [] =>
                acc
           | [x] =>
                x :: acc
           | [x, y] =>
                (case f (x, y) of
                    GREATER =>
                       y :: x :: acc
                  | _ =>
                       x :: y :: acc)
           | pivot :: rest =>
                let
                   val (left, middle, right) = split f pivot rest [] [pivot] []
                in
                   sortOntoBwd f left (revOnto middle (sortOntoBwd f right acc))
                end)

      (* antistable *)
      and sortOntoBwd f l acc =
         (case l of
             [] =>
                acc
           | [x] =>
                x :: acc
           | [x, y] =>
                (case f (x, y) of
                    LESS =>
                       x :: y :: acc
                  | _ =>
                       y :: x :: acc)
           | pivot :: rest =>
                let
                   val (left, middle, right) = split f pivot rest [] [pivot] []
                in
                   sortOntoFwd f left (middle @ sortOntoFwd f right acc)
                end)

      fun sort f l = sortOntoFwd f l []

   end

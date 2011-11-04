(* Adapted from Chris Okasaki, Robert Harper, and Tom Murphy VII's position
 * library, as well as Kaustuv Chaudhur's Mark library for the L2/C0
 * compiler. *)

structure Pos :> POS = 
   struct
      type pos = Coord.t * Coord.t
      type t = pos

      fun toString (left, right) = 
         Coord.toString left ^ "-" 
         ^ Int.toString (Coord.line right) ^ "."
         ^ Int.toString (Coord.char right)

      fun pos left right = 
         (case Coord.compare (left, right) of
            GREATER => (right, left)
          | _ => (left, right))

      fun left (left, right) = left
  
      fun right (left, right) = right

      fun union (l1, r1) (l2, r2) = 
         (Coord.leftmost l1 l2, Coord.rightmost r1 r2)

      fun max (l1, r1) (l2, r2) = 
         (Coord.rightmost l1 l2, Coord.rightmost r1 r2)

      fun min (l1, r1) (l2, r2) = 
         (Coord.leftmost l1 l2, Coord.rightmost r1 r2)
   end

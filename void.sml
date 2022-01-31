
structure Void :> VOID =
   struct
      type void = unit

      val absurd = fn _ => raise Match
   end

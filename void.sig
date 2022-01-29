
signature VOID =
(* Nullary sum type *)
   sig

      type void

      val absurd : void -> 'a

   end

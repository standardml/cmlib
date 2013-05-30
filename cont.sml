
(* A bogus implementation of CONT, for platforms that don't support call/cc. *)

structure Cont :> CONT =
   struct

      type 'a cont = unit

      fun callcc f = f ()

      fun throw () x =
         raise (Fail "Continuations not implemented.")

   end

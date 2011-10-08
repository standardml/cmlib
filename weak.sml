
(* A semantically sound but unhelpful implementation of WEAK,
   for platforms that don't support weak pointers.
*)

structure Weak :> WEAK =
   struct

      type 'a weak = 'a

      fun weak x = x
      fun strong x = SOME x

   end

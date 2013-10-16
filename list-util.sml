
structure ListUtil :> LIST_UTIL =
   struct

      fun foldrLazy f x l =
         (case l of
             [] => x
           | h :: t =>
                f (h, Susp.delay (fn () => foldrLazy f x t)))

   end
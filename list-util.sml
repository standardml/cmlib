
structure ListUtil :> LIST_UTIL =
   struct

      fun revapp f l =
         (case l of
             nil => ()

           | h :: t =>
                (
                revapp f t;
                f h
                ))

      fun foldrLazy f x l =
         (case l of
             [] => x
           | h :: t =>
                f (h, Susp.delay (fn () => foldrLazy f x t)))

   end
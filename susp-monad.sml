
structure SuspMonad :> MONAD where type 'a m = 'a Susp.susp =
   struct

      type 'a m = 'a Susp.susp

      fun return x = Susp.delay (fn () => x)

      fun seq s1 s2 =
         Susp.delay (fn () => (Susp.force s1; Susp.force s2))

      fun bind s1 s2 =
         Susp.delay
         (fn () =>
             let
                val x = Susp.force s1
             in
                Susp.force (s2 x)
             end)

   end

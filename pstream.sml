
structure PStream
   :> PSTREAM
   =
   struct

      open PSusp

      datatype 'a front = Nil | Cons of 'a * 'a pstream
      withtype 'a pstream = 'a front psusp

      val front = force
      fun eager fr = delay (fn () => SOME fr)
      val lazy = delay

      (* Use NotReady, isReady, and poll directly from PSusp. *)
      
      datatype 'a result =
         Ready of 'a
       | Blocked
       | Closed

      fun fromBlockingProcess f =
         lazy
         (fn () =>
             (case f () of
                 Ready x =>
                    SOME (Cons (x, fromBlockingProcess f))
               | Blocked =>
                    NONE
               | Closed =>
                    SOME Nil))

      fun fromIqueue q =
         lazy
         (fn () =>
             if IQueue.isEmpty q then
                NONE
             else
                SOME (Cons (IQueue.remove q, fromIqueue q)))

      fun toStream s =
         Stream.lazy
         (fn () =>
             (case poll s of
                 SOME Nil =>
                    Stream.Nil
               | SOME (Cons (x, s')) =>
                    Stream.Cons (SOME x, toStream s')
               | NONE =>
                    Stream.Cons (NONE, toStream s)))

      fun fix f = f (lazy (fn () => poll (fix f)))

      fun map f s =
         lazy
         (fn () =>
             (case poll s of
                 SOME Nil =>
                    SOME Nil
               | SOME (Cons (x, s')) =>
                    SOME (Cons (f x, map f s'))
               | NONE =>
                    NONE))

   end

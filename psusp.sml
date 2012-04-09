
structure PSusp :> PSUSP =
   struct

      type 'a psusp = (unit -> 'a option) ref

      fun delay f =
         let
            val r = ref (fn () => raise (Fail "empty suspension"))
               
            val () =
               r := (fn () => let
                                 val res = f ()

                                 val () =
                                    (case res of
                                        SOME _ =>
                                           r := (fn () => res)
                                      | NONE =>
                                           ())
                              in
                                 res
                              end)
         in
            r
         end

      exception NotReady

      fun force r =
         (case !r () of
             SOME x => x
           | NONE =>
                raise NotReady)

      fun isReady r = Option.isSome (!r ())

      fun poll r = !r ()

   end

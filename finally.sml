
structure Finally :> FINALLY =
   struct

      fun finally f g =
         let
            val x = Sum.INL (f ()) handle exn => Sum.INR exn
         in
            g ();
            (case x of
                Sum.INL y => y
              | Sum.INR exn => raise exn)
         end

   end
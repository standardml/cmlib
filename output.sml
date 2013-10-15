
structure Output :> OUTPUT =
   struct

      (* The consumer does two things, determined by its option argument.  If the option
         is NONE, it returns its current state out-of-band (using an exception).  If the
         option is (SOME x), it returns a new consumer with its state updated using x.

         This could be done using two functions, but it seems better to allocate just
         one closure, than two closures and a pair.
      *)
      datatype 'a consumer =
         C of ('a option -> 'a consumer)

      type ('a, 'b) outputm = 'a consumer -> 'b * 'a consumer
      type 'a output = ('a , unit) outputm

      fun nothing c = ((), c)

      fun output x (C f) = ((), f (SOME x))

      fun return x c = (x, c)

      fun seq out1 out2 c =
         let
            val (_, c') = out1 c
         in
            out2 c'
         end

      fun lazy f c = f () c

      fun bind out f c =
         let
            val (x, c') = out c
         in
            f x c'
         end

      fun exec out extfst extrest (init : 'a) =
         let
            exception E of 'a

            fun loop st msg =
               (case msg of
                   NONE => raise (E st)
                 | SOME x =>
                      C (loop (extrest (x, st))))

            fun loop' st msg =
               (case msg of
                   NONE => raise (E st)
                 | SOME x =>
                      C (loop (extfst (x, st))))

            val (ans, C f) = out (C (loop' init))
         in
            (f NONE; raise (Fail "dead code"))
            handle E final => (ans, final)
         end

      fun fold f init out =
         let
            val ((), res) = exec out f f init
         in
            res
         end

      fun append out = rev (fold (op ::) [] out)

      fun app f out = fold (fn (x, ()) => f x) () out

      fun appWith sep f out =
         let
            val ((), res) =
               exec out
               (fn (x, ()) => f x)
               (fn (x, ()) => (sep (); f x))
               ()
         in
            res
         end

   end


functor OutputMonad (type elem)
   :>
   MONAD
   where type 'a m = (elem, 'a) Output.outputm
   =
   struct
      type 'a m = (elem, 'a) Output.outputm

      val return = Output.return
      val seq = Output.seq
      val bind = Output.bind
   end

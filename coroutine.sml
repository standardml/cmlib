
structure Coroutine 
   :> COROUTINE
   =
   struct

      open Cont

      datatype ('a, 'b) corout = 
         C of ('a * ('b, 'a) corout) cont

      fun coroutine f =
         callcc
         (fn exit =>
             f (callcc (fn k => throw exit (C k))))

      fun yield (C k) x =
         callcc
         (fn kSelf => throw k (x, C kSelf))

   end

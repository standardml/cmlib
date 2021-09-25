
structure VectorSliceStreamable
   :> STREAMABLE
      where type 'a t = 'a VectorSlice.slice
   =
   struct

      type 'a t = 'a VectorSlice.slice

      datatype 'a front = Nil | Cons of 'a * 'a t

      fun front v =
         (case VectorSlice.getItem v of
             NONE => Nil
           | SOME (h, t) => Cons (h, t))

   end


functor MonomorphizeStreamable (structure Streamable : STREAMABLE
                                type elem)
   :> MONO_STREAMABLE
      where type t = elem Streamable.t
      where type elem = elem
   =
   struct

      type t = elem Streamable.t
      type elem = elem

      datatype front = Nil | Cons of elem * t

      fun front s =
         (case Streamable.front s of
             Streamable.Nil => Nil
           | Streamable.Cons (x, s') => Cons (x, s'))

   end

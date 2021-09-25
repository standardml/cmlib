
structure ListStreamable
   :> STREAMABLE
      where type 'a t = 'a list
   =
   struct

      type 'a t = 'a list
      datatype 'a front = Nil | Cons of 'a * 'a t

      fun front l =
         (case l of
             [] => Nil
           | h :: t => Cons (h, t))

   end


structure StreamStreamable
   :> STREAMABLE
      where type 'a t = 'a Stream.stream
   =
   struct

      type 'a t = 'a Stream.stream
      open Stream

   end


functor CoercedStreamable (structure Streamable : STREAMABLE
                           type 'a item
                           val coerce : 'a item -> 'a)
   :> STREAMABLE
      where type 'a t = 'a item Streamable.t
   =
   struct

      type 'a t = 'a item Streamable.t

      datatype 'a front = Nil | Cons of 'a * 'a item Streamable.t

      fun front s =
         (case Streamable.front s of
             Streamable.Nil => Nil
           | Streamable.Cons (x, s') =>
                Cons (coerce x, s'))

   end

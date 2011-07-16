
structure ListStreamable
   :> STREAMABLE
      where type 'a t = 'a list
   =
   struct

      type 'a t = 'a list
      datatype 'a front = Nil | Cons of 'a * 'a list

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
      datatype front = datatype Stream.front
      val front = Stream.front

   end


signature STREAMABLE =
   sig
      type 'a t

      datatype 'a front = Nil | Cons of 'a * 'a t
      val front : 'a t -> 'a front
   end

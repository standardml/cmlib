
signature MONO_STREAMABLE =
   sig
      type t
      type elem

      datatype front = Nil | Cons of elem * t
      val front : t -> front
   end


signature HASHABLE =
   sig
      type t

      val eq : t * t -> bool
      val hash : t -> word
   end


signature ORDERED =
   sig
      type t
         
      val eq : t * t -> bool
      val compare : t * t -> order
   end

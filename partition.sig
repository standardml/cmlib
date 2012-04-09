
signature PARTITION =
   sig

      type 'a seq
      val partition : ('a * 'a -> bool) -> 'a seq -> 'a seq seq

   end

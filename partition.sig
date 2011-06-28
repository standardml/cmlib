
signature PARTITION =
   sig

      val partition : ('a * 'a -> bool) -> 'a list -> 'a list list

   end

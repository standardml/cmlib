
signature MONAD =
   sig

      type 'a m

      val return : 'a -> 'a m
      val seq : 'a m -> 'b m -> 'b m
      val bind : 'a m -> ('a -> 'b m) -> 'b m

   end

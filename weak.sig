
signature WEAK =
   sig

      type 'a weak
      val weak : 'a -> 'a weak
      val strong : 'a weak -> 'a option

   end

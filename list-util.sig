
signature LIST_UTIL =
   sig

      val foldrLazy : ('a * 'b Susp.susp -> 'b) -> 'b -> 'a list -> 'b

   end


signature ARRAY_UTIL =
   sig

      val foldliLazy : (int * 'a * 'b Susp.susp -> 'b) -> 'b -> 'a array -> 'b
      val foldriLazy : (int * 'a * 'b Susp.susp -> 'b) -> 'b -> 'a array -> 'b
      val foldlLazy : ('a * 'b Susp.susp -> 'b) -> 'b -> 'a array -> 'b
      val foldrLazy : ('a * 'b Susp.susp -> 'b) -> 'b -> 'a array -> 'b

   end

signature LIST_UTIL =
   sig

      val revapp : ('a -> unit) -> 'a list -> unit
      val foldrLazy : ('a * 'b Susp.susp -> 'b) -> 'b -> 'a list -> 'b

   end

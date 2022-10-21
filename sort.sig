
signature SORT =
   sig
      val sort : ('a * 'a -> order) -> 'a list -> 'a list
   end

signature FULLSORT =
   sig
      include SORT

      val sort' : ('a * 'a -> bool) -> 'a list -> 'a list
   end

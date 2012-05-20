
signature IMPERATIVE_UNION_FIND =
   sig

      type 'a set

      val new : 'a -> 'a set
      val eq : 'a set * 'a set -> bool
      val union : ('a * 'a -> 'a) -> 'a set -> 'a set -> unit
      val sameSet : 'a set * 'a set -> bool
      val find : 'a set -> 'a
      val isCanonical : 'a set -> bool
      
   end

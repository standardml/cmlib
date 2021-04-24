
signature TOPOLOGICAL_SORT =
   sig

      type key
      type value

      exception Cycle of key

      val sort : value list -> value list

   end
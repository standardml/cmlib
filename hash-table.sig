
signature HASH_TABLE =
   sig

      type key
      type 'a table

      exception Absent

      val table : int -> 'a table
      val member : 'a table -> key -> bool
      val insert : 'a table -> key -> 'a -> unit
      val find : 'a table -> key -> 'a option
      val lookup : 'a table -> key -> 'a
      val lookupOrInsert : 'a table -> key -> 'a -> 'a option

      val fold : (key * 'a * 'b -> 'b) -> 'b -> 'a table -> 'b

   end

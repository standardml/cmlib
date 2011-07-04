
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
      val lookupOrInsert : 'a table -> key -> (unit -> 'a) -> 'a

      val fold : (key * 'a * 'b -> 'b) -> 'b -> 'a table -> 'b
      val app : (key * 'a -> unit) -> 'a table -> unit

   end


signature HASH_TABLE =
   sig

      type key
      type 'a table

      exception Absent

      val table : int -> 'a table
      val reset : 'a table -> int -> unit

      val member : 'a table -> key -> bool
      val insert : 'a table -> key -> 'a -> unit
      val remove : 'a table -> key -> unit
      val find : 'a table -> key -> 'a option
      val lookup : 'a table -> key -> 'a

      val operate : 'a table -> key -> (unit -> 'a) -> ('a -> 'a) -> 'a option * 'a
      val insertMerge : 'a table -> key -> 'a -> ('a -> 'a) -> unit
      val lookupOrInsert : 'a table -> key -> (unit -> 'a) -> 'a

      val toList : 'a table -> (key * 'a) list
      val fold : (key * 'a * 'b -> 'b) -> 'b -> 'a table -> 'b
      val app : (key * 'a -> unit) -> 'a table -> unit

   end


(* This isn't just a set.  The idea is the data is part of the key, but
   is ignored by the hash and eq functions used to construct the table.
*)

signature DATALESS_HASH_TABLE =
   sig

      type key
      type table

      exception Absent

      val table : int -> table
      val reset : table -> int -> unit

      val size : table -> int
      val member : table -> key -> bool
      val insert : table -> key -> unit
      val remove : table -> key -> unit
      val find : table -> key -> key option
      val lookup : table -> key -> key

      val lookupOrInsert : table -> key -> key
      val lookupOrInsert' : table -> key -> key * bool   (* true if already present *)

      val toList : table -> key list
      val fold : (key * 'b -> 'b) -> 'b -> table -> 'b
      val app : (key -> unit) -> table -> unit

   end

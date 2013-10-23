
(* This isn't just a set.  The idea is the data is part of the key, but
   is ignored by the hash and eq functions used to construct the table.
*)

signature DATALESS_IDICT =
   sig

      type init
      type key
      type table

      exception Absent

      val table : init -> table
      val reset : table -> init -> unit

      val size : table -> int
      val member : table -> key -> bool
      val insert : table -> key -> unit
      val remove : table -> key -> unit
      val find : table -> key -> key option
      val lookup : table -> key -> key

      val lookupOrInsert : table -> key -> key
      val lookupOrInsert' : table -> key -> key * bool   (* true if already present *)

      val swap : table -> key -> key option  (* insert and return old entry, if any *)

      val toList : table -> key list
      val fold : (key * 'b -> 'b) -> 'b -> table -> 'b
      val foldLazy : (key * 'b Susp.susp -> 'b) -> 'b -> table -> 'b
      val app : (key -> unit) -> table -> unit

   end


signature MINI_DATALESS_IDICT =
   sig

      type init
      type key
      type table

      val table : init -> table
      val reset : table -> init -> unit

      val size : table -> int
      val insert : table -> key -> unit
      val remove : table -> key -> unit
      val find : table -> key -> key option

      val fold : (key * 'b -> 'b) -> 'b -> table -> 'b

   end

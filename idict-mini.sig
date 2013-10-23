
signature MINI_IDICT =
   sig

      type init
      type key
      type 'a table

      val table : init -> 'a table
      val reset : 'a table -> init -> unit

      val size : 'a table -> int
      val insert : 'a table -> key -> 'a -> unit
      val remove : 'a table -> key -> unit
      val find : 'a table -> key -> 'a option

      val fold : (key * 'a * 'b -> 'b) -> 'b -> 'a table -> 'b

   end

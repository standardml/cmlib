
signature TABLE = 
   sig
 
      type key
      type value
      type checkpoint
      
      val member : key -> bool
      val insert : key -> value -> unit
      val remove : key -> unit
      val find : key -> value option
      val lookup : key -> value

      val operate : 
         key -> (unit -> value) -> (value -> value) -> value option * value
      val insertMerge : key -> value -> (value -> value) -> unit

      val toList : unit -> (key * value) list
      val fold : (key * value * 'a -> 'a) -> 'a -> 'a 
      val app : (key * value -> unit) -> unit

      val reset : unit -> unit
      val save : unit -> checkpoint
      val restore : checkpoint -> unit

end

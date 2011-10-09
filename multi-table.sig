
signature MULTI_TABLE = 
   sig
 
      type key
      type value
      
      val reset : unit -> unit
      val member : key -> bool
      val insert : key -> value -> unit
      val find : key -> value list
      val lookup : key -> value list
      val toList : unit -> (key * value list) list

end

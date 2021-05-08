
signature ARRAY_INF =
   sig
      type 'a array
      
      val array : 'a -> 'a array
      val sub : 'a array * int -> 'a
      val update : 'a array * int * 'a -> unit
      val erase : 'a array -> unit
      val isEmpty : 'a array -> bool
   end

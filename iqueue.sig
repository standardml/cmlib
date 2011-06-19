
signature IQUEUE =
   sig
      type 'a iqueue

      val iqueue : unit -> 'a iqueue
      val reset : 'a iqueue -> unit

      exception Empty
      val insert : 'a iqueue -> 'a -> unit
      val isEmpty : 'a iqueue -> bool
      val front : 'a iqueue -> 'a
      val remove : 'a iqueue -> 'a
   end

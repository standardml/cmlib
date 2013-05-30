
signature IDEQUE =
   sig
      type 'a ideque
      type idequeNode

      val ideque : unit -> 'a ideque
      val reset : 'a ideque -> unit

      exception Empty
      val insertFront : 'a ideque -> 'a -> unit
      val insertBack : 'a ideque -> 'a -> unit
      val isEmpty : 'a ideque -> bool
      val front : 'a ideque -> 'a
      val back : 'a ideque -> 'a
      val removeFront : 'a ideque -> 'a
      val removeBack : 'a ideque -> 'a

      (* same as insertFront/insertBack, but returns a node for deletion *)
      val insertFrontNode : 'a ideque -> 'a -> idequeNode
      val insertBackNode : 'a ideque -> 'a -> idequeNode

      (* NB: resetting an ideque does not necessarily mark all its elements as orphans. *)
      exception Orphan
      val delete : idequeNode -> unit
      val dummy : idequeNode
      val orphan : idequeNode -> bool
   end

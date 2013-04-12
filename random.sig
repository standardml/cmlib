
signature RANDOM =
   sig

      type data

      val random : int -> data
      val addEntropy : Bytestring.string -> unit      
      val reseed : unit -> unit

   end

      
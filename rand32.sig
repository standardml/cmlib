
signature RAND32 =
   sig
      val randWord32 : unit -> Word32.word

      type seed
      val reseed : seed -> unit
   end

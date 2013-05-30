
signature RAND =
   sig

      (* randomX(max) generates a random number in the range 0 .. max-1 *)
      val randIntInf : IntInf.int -> IntInf.int
      val randInt : int -> int
      
      val randBool : unit -> bool
      val randBits : int -> IntInf.int
      val randWord8 : unit -> Word8.word

      val randWord32 : unit -> Word32.word

      type seed
      val reseed : seed -> unit

   end

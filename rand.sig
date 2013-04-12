
signature RAND =
   sig

      (* randomX(max) generates a random number in the range 0 .. max-1 *)
      val randomIntInf : IntInf.int -> IntInf.int
      val randomInt : int -> int
      
      val randomBits : int -> IntInf.int
      val randomWord8 : unit -> Word8.word

   end

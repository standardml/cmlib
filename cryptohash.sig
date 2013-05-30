
signature CRYPTO_HASH =
   sig

      type state

      val initial : state
      val update : state * Bytestring.string -> state
      val finish : state * Word8.word Stream.stream -> Bytestring.string

      val hash : Word8.word Stream.stream -> Bytestring.string

   end

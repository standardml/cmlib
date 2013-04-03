
signature CRYPTO_HASH =
   sig

      val hash : Word8.word Stream.stream -> Bytestring.string

   end

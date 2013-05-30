
functor CBCCipherFun (structure Cipher : CIPHER)
   :>
   CIPHER
   where type prekey = Cipher.key
   where type init = Bytestring.string
   =
   struct

      structure B = Bytestring

      type prekey = Cipher.key
      type init = B.string
      type key = Cipher.key * B.string ref

      val sz =
         (case Cipher.messageSize of
             SOME sz => sz
           | NONE =>
                raise (Fail "CBCCipherFun operates only on ciphers with fixed message lengths."))

      val messageSize = Cipher.messageSize

      fun makeKey (key, init) =
         if B.size init <> sz then
            raise Domain
         else
            (key, ref init)
      
      fun encrypt ((key, initr), plaintext) =
         if B.size plaintext <> sz then
            raise Domain
         else
            let
               val ciphertext = Cipher.encrypt (key, B.map2 Word8.xorb (plaintext, !initr))
            in
               initr := ciphertext;
               ciphertext
            end

      fun decrypt ((key, initr), ciphertext) =
         if B.size ciphertext <> sz then
            raise Domain
         else
            let
               val plaintext = B.map2 Word8.xorb (Cipher.decrypt (key, ciphertext), !initr)
            in
               initr := ciphertext;
               plaintext
            end

   end


functor CFBCipherFun (structure Cipher : CIPHER)
   :>
   CIPHER
   where type prekey = Cipher.key
   where type init = Bytestring.string
   =
   struct

      structure B = Bytestring

      type prekey = Cipher.key
      type init = B.string
      type key = Cipher.key * B.string ref

      val sz =
         (case Cipher.messageSize of
             SOME sz => sz
           | NONE =>
                raise (Fail "CFBCipherFun operates only on ciphers with fixed message lengths."))

      val messageSize = Cipher.messageSize

      fun makeKey (key, init) =
         if B.size init <> sz then
            raise Domain
         else
            (key, ref init)
      
      fun encrypt ((key, initr), plaintext) =
         if B.size plaintext <> sz then
            raise Domain
         else
            let
               val ciphertext = B.map2 Word8.xorb (Cipher.encrypt (key, !initr), plaintext)
            in
               initr := ciphertext;
               ciphertext
            end

      fun decrypt ((key, initr), ciphertext) =
         if B.size ciphertext <> sz then
            raise Domain
         else
            let
               val plaintext = B.map2 Word8.xorb (Cipher.encrypt (key, !initr), ciphertext)
            in
               initr := ciphertext;
               plaintext
            end

   end


functor OFBCipherFun (structure Cipher : CIPHER)
   :>
   CIPHER
   where type prekey = Cipher.key
   where type init = Bytestring.string
   =
   struct

      structure B = Bytestring

      type prekey = Cipher.key
      type init = B.string
      type key = Cipher.key * B.string ref

      val sz =
         (case Cipher.messageSize of
             SOME sz => sz
           | NONE =>
                raise (Fail "OFBCipherFun operates only on ciphers with fixed message lengths."))

      val messageSize = Cipher.messageSize

      fun makeKey (key, init) =
         if B.size init <> sz then
            raise Domain
         else
            (key, ref init)
      
      fun encrypt ((key, initr), msg) =
         if B.size msg <> sz then
            raise Domain
         else
            let
               val bits = Cipher.encrypt (key, !initr)
            in
               initr := bits;
               B.map2 Word8.xorb (bits, msg)
            end

      val decrypt = encrypt

   end


functor CTRCipherFun (structure Cipher : CIPHER)
   :>
   sig
      include CIPHER
      where type prekey = Cipher.key
      where type init = Bytestring.string
   end
   =
   struct

      structure B = Bytestring

      type prekey = Cipher.key
      type init = B.string
      type key = Cipher.key * IntInf.int ref

      val sz =
         (case Cipher.messageSize of
             SOME sz => sz
           | NONE =>
                raise (Fail "CTRCipherFun operates only on ciphers with fixed message lengths."))

      val max = IntInf.<< (1, Word.fromInt (sz*8))

      val messageSize = Cipher.messageSize

      fun makeKey (key, init) =
         if B.size init <> sz then
            raise Domain
         else
            (key, ref (ConvertIntInf.fromBytesB init))
      
      fun encrypt ((key, initr), msg) =
         if B.size msg <> sz then
            raise Domain
         else
            let
               val bits = Cipher.encrypt (key, ConvertIntInf.toFixedBytesB (sz, !initr))
            in
               initr := (!initr + 1) mod max;
               B.map2 Word8.xorb (bits, msg)
            end

      val decrypt = encrypt

   end

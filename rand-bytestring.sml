
functor BytestringRand (structure Random : RANDOM where type data = Bytestring.string) :> RAND =
   struct

      fun randomIntInf max =
         if max <= 0 then
            raise Domain
         else
            let
               val bytes = (IntInf.log2 max) div 8 + 1
               val limit = IntInf.div (IntInf.<< (1, Word.fromInt (bytes*8)) - 1, max)

               fun loop () =
                  let 
                     val r = ConvertIntInf.fromBytesB (Random.random bytes)

                     val (q, m) = IntInf.divMod (r, max)
                  in
                     if q >= limit then
                        loop ()
                     else
                        m
                  end
            in
               loop ()
            end

      (* If we knew the size of an int, we could hardcode something better. *)
      fun randomInt max =
         IntInf.toInt (randomIntInf (IntInf.fromInt max))

      fun randomWord8 () =
         hd (Bytestring.explode (Random.random 1))

      fun randomBits n =
         let
            val h = Word8.andb (Word8.<< (0w1, Word.fromInt (n mod 8)) - 0w1, randomWord8 ())
            val t = Random.random (n div 8)
         in
            ConvertIntInf.fromBytesB (Bytestring.^ (Bytestring.str h, t))
         end

   end

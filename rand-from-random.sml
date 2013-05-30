
functor RandFromRandom (structure Random : RANDOM) 
   :>
   RAND
   where type seed = Random.seed
   =
   struct

      fun randWord8 () =
         hd (Bytestring.explode (Random.random 1))

      fun randBool () =
         Word8.andb (randWord8 (), 0w1) = 0w0

      fun randBits n =
         let
            val h = Word8.andb (Word8.<< (0w1, Word.fromInt (n mod 8)) - 0w1, randWord8 ())
            val t = Random.random (n div 8)
         in
            ConvertIntInf.fromBytesB (Bytestring.^ (Bytestring.str h, t))
         end

      fun randIntInf max =
         if max <= 0 then
            raise Domain
         else
            let
               val x = randBits (IntInf.log2 max + 1)
            in
               if x >= max then
                  randIntInf max
               else
                  x
            end

      (* If we knew the size of an int, we could hardcode something better than this. *)
      fun randInt max =
         IntInf.toInt (randIntInf (IntInf.fromInt max))

      fun randWord32 () =
         ConvertWord.bytesToWord32B (Random.random 4)

      type seed = Random.seed
      val reseed = Random.reseed

   end

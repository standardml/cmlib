
functor RandFromRand32 (structure Rand32 : RAND32)
   :>
   RAND
   where type seed = Rand32.seed
   =
   struct

      open Rand32

      fun randBool () = Word32.andb (randWord32 (), 0w1) = 0w0

      fun randWord8 () = ConvertWord.word32ToWord8 (randWord32 ())

      fun randBits bits =
         let
            fun loop acc n =
               if n = 0 then
                  acc
               else
                  IntInf.orb (IntInf.<< (acc, 0w32), ConvertWord.word32ToIntInf (randWord32 ()))

            val words = bits div 32
            val extra = bits mod 32
         in
            if extra = 0 then
               loop 0 words
            else
               loop 
               (ConvertWord.word32ToIntInf
                   (Word32.andb (randWord32 (), Word32.<< (0w1, Word.fromInt extra) - 0w1)))
               words
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

   end

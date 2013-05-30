
(* According to Wikipedia, this isn't a good secure random number generator. *)

structure RehashRandom
   :> 
   RANDOM
   where type seed = Bytestring.string
   =
   struct

      fun hash str = SHA256.hashBytes (SHA256.hashBytes str)

      val seed = ref (hash (ConvertWord.wordToBytesB (Word.fromLargeInt (Time.toMilliseconds (Time.now ())))))

      fun random16 () =
         let
            val s = !seed
         in
            seed := hash s;
            Bytestring.substring (s, 0, 16)
         end

      fun random n =
         let
            fun loop acc n =
               if n <= 16 then
                  Bytestring.substring (random16 (), 0, n) :: acc
               else
                  loop (random16 () :: acc) (n-16)
         in
            if n < 0 then
               raise Domain
            else
               Bytestring.concat (loop [] n)
         end

      type seed = Bytestring.string

      fun reseed str =
         seed := hash (Bytestring.^ (!seed, str))

   end


structure SimpleRandom :> RANDOM where type data = Bytestring.string =
   struct

      (* According to Wikipedia, this isn't a good secure random number generator, but it's
         simple and it's better than nothing.  We should replace this eventually.
      *)

      type data = Bytestring.string

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

      fun addEntropy str =
         seed := hash (Bytestring.^ (!seed, str))

      fun reseed () = ()

   end

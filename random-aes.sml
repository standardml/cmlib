
structure AESRandom 
   :> 
   RANDOM
   where type seed = Bytestring.string
   =
   struct

      structure B = Bytestring

      fun hash str = SHA256.hashBytes (SHA256.hashBytes str)

      val maxalloc = 1048576  (* 1 MB *)
      val blocksz = 16

      val currKey = 
         ref (AES.makeKey (hash (ConvertWord.wordToBytesB (Word.fromLargeInt (Time.toMilliseconds (Time.now ())))), ()))
      
      val currCounter : IntInf.int ref = ref 1

      fun randomBlock () =
         let
            val block = AES.encrypt (!currKey, ConvertIntInf.toFixedBytesB (blocksz, !currCounter))
         in
            (* We can neglect the possibility that the counter will overflow 2^256. *)
            currCounter := !currCounter + 1;
            block
         end

      fun randomCore acc bytes =
         let
            fun loop acc n =
               if n > blocksz then
                  loop (randomBlock () :: acc) (n-blocksz)
               else
                  B.substring (randomBlock (), 0, n) :: acc

            val acc' = loop acc bytes
         in
            currKey := AES.makeKey (B.^ (randomBlock (), randomBlock ()), ());
            acc'
         end

      fun random bytes =
         let
            fun loop acc n =
               if n > maxalloc then
                  loop (randomCore acc maxalloc) (n-maxalloc)
               else
                  randomCore acc n
         in
            B.concat (loop [] bytes)
         end

      type seed = Bytestring.string

      fun reseed s =
         currKey := AES.makeKey (hash (B.concat [randomBlock (), randomBlock (), s]), ())

   end

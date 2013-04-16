
structure ConvertWord : CONVERT_WORD =
   struct

      type word = Word.word
      type word8 = Word8.word
      type word31 = Word31.word
      type word32 = Word32.word
      type word64 = Word64.word


      (* This stuff all depends on the size of LargeWord. *)

      fun word8ToWord31 w = Word31.fromLarge (Word8.toLarge w)
      fun word8ToWord31X w = Word31.fromLarge (Word8.toLargeX w)
      val word8ToWord32 = Word8.toLarge
      val word8ToWord32X = Word8.toLargeX
      fun word8ToWord64 w = Word64.fromInt (Word8.toInt w)
      val word8ToIntInf = Word8.toLargeInt

      fun word8ToWord64X w =
         let
            val x = word8ToWord64 w
            
            open Word64
         in
            orb (x, andb (0wxffffffffffffff00, xorb (>> (andb (x, 0wx80), 0w7), 0w1) - 0w1))
         end

      fun word31ToWord8 w = Word8.fromLarge (Word31.toLarge w)
      val word31ToWord32 = Word31.toLarge
      val word31ToWord32X = Word31.toLargeX
      val word31ToIntInf = Word31.toLargeInt

      fun word31ToWord64 w = 
         (* probably inefficient, since LargeInt is infinite precision *)
         Word64.fromLargeInt (Word31.toLargeInt w)

      fun word31ToWord64X w =
         let
            val x = word31ToWord64 w
            
            open Word64
         in
            orb (x, andb (0wxFFFFFFFF80000000, xorb (>> (andb (x, 0wx40000000), 0w30), 0w1) - 0w1))
         end


      val word32ToWord8 = Word8.fromLarge
      val word32ToWord31 = Word31.fromLarge
      fun word32ToWord64 w = Word64.fromLargeInt (Word32.toLargeInt w)
      val word32ToIntInf = Word32.toLargeInt

      fun word32ToWord64X w =
         let
            val x = word32ToWord64 w
            
            open Word64
         in
            orb (x, andb (0wxFFFFFFFF00000000, xorb (>> (andb (x, 0wx80000000), 0w31), 0w1) - 0w1))
         end

      fun word64ToWord8 w = Word8.fromInt (Word64.toInt (Word64.andb (w, 0wxff)))
      fun word64ToWord31 w = Word31.fromLargeInt (Word64.toLargeInt (Word64.andb (w, 0wx7fffffff)))
      fun word64ToWord32 w = Word32.fromLargeInt (Word64.toLargeInt (Word64.andb (w, 0wxffffffff)))
      val word64ToIntInf = Word64.toLargeInt


      val intInfToWord8 = Word8.fromLargeInt
      val intInfToWord31 = Word31.fromLargeInt
      val intInfToWord32 = Word32.fromLargeInt
      val intInfToWord64 = Word64.fromLargeInt


      fun word32ToBytesB w =
         let
            val a = Word8Array.array (4, 0w0)
         in
            PackWord32Big.update (a, 0, w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end

      fun word32ToBytesL w =
         let
            val a = Word8Array.array (4, 0w0)
         in
            PackWord32Little.update (a, 0, w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end

      fun word31ToBytesB w =
         word32ToBytesB (word31ToWord32 w)

      fun word31ToBytesL w =
         word32ToBytesL (word31ToWord32 w)

      fun word64ToBytesB w =
         let
            val a = Word8Array.array (8, 0w0)
         in
            PackWord32Big.update (a, 0, word64ToWord32 (Word64.>> (w, 0w32)));
            PackWord32Big.update (a, 1, word64ToWord32 w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end

      fun word64ToBytesL w =
         let
            val a = Word8Array.array (8, 0w0)
         in
            PackWord32Big.update (a, 1, word64ToWord32 (Word64.>> (w, 0w32)));
            PackWord32Big.update (a, 0, word64ToWord32 w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end
         

      exception ConvertWord

      fun bytesToWord31B s =
         if Bytestring.size s <> 4 orelse Word8.> (Bytestring.sub (s, 0), 0wx7f) then
            raise ConvertWord
         else
            word32ToWord31 (PackWord32Big.subVec (Bytestring.toWord8Vector s, 0))

      fun bytesToWord31L s =
         if Bytestring.size s <> 4 orelse Word8.> (Bytestring.sub (s, 3), 0wx7f) then
            raise ConvertWord
         else
            word32ToWord31 (PackWord32Little.subVec (Bytestring.toWord8Vector s, 0))

      fun bytesToWord32B s =
         if Bytestring.size s <> 4 then
            raise ConvertWord
         else
            PackWord32Big.subVec (Bytestring.toWord8Vector s, 0)

      fun bytesToWord32L s =
         if Bytestring.size s <> 4 then
            raise ConvertWord
         else
            PackWord32Little.subVec (Bytestring.toWord8Vector s, 0)

      fun bytesToWord64B s =
         if Bytestring.size s <> 8 then
            raise ConvertWord
         else
            let
               val v = Bytestring.toWord8Vector s
            in
               Word64.orb (Word64.<< (word32ToWord64 (PackWord32Big.subVec (v, 0)), 0w32),
                           word32ToWord64 (PackWord32Big.subVec (v, 1)))
            end

      fun bytesToWord64L s =
         if Bytestring.size s <> 8 then
            raise ConvertWord
         else
            let
               val v = Bytestring.toWord8Vector s
            in
               Word64.orb (Word64.<< (word32ToWord64 (PackWord32Little.subVec (v, 1)), 0w32),
                           word32ToWord64 (PackWord32Little.subVec (v, 0)))
            end


      (* This stuff depends on the size of Word. *)

      val wordToWord8 = word31ToWord8
      fun wordToWord31 w = w
      val wordToWord32 = word31ToWord32
      val wordToWord32X = word31ToWord32X
      val wordToWord64 = word31ToWord64
      val wordToWord64X = word31ToWord64X
      val wordToIntInf = word31ToIntInf
      val wordToBytesB = word31ToBytesB
      val wordToBytesL = word31ToBytesL

      val word8ToWord = word8ToWord31
      val word8ToWordX = word8ToWord31X
      fun word31ToWord w = w
      fun word31ToWordX w = w
      val word32ToWord = word32ToWord31
      val word32ToWordX = word32ToWord31
      val word64ToWord = word64ToWord31
      val word64ToWordX = word64ToWord31
      val intInfToWord = intInfToWord31
      val bytesToWordB = bytesToWord31B
      val bytesToWordL = bytesToWord31L

   end

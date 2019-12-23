
structure ConvertWord : CONVERT_WORD =
   struct

      type word = Word.word
      type wordlg = LargeWord.word
      type word8 = Word8.word
      type word32 = Word32.word
      type word64 = Word64.word


      (* A lot of this could be simpler if LargeWord were the largest word size. *)


      (* This stuff all depends on the size of LargeWord. *)

      val word8ToWord32 = Word32.fromLarge o Word8.toLarge
      val word8ToWord32X = Word32.fromLarge o Word8.toLargeX
      fun word8ToWord64 w = Word64.fromInt (Word8.toInt w)
      val word8ToIntInf = Word8.toLargeInt

      fun word8ToWord64X w =
         let
            val x = word8ToWord64 w

            open Word64
         in
            orb (x, andb (0wxffffffffffffff00, xorb (>> (andb (x, 0wx80), 0w7), 0w1) - 0w1))
         end

      val word32ToWord8 = Word8.fromLarge o Word32.toLarge
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
      fun word64ToWord32 w = Word32.fromLargeInt (Word64.toLargeInt (Word64.andb (w, 0wxffffffff)))
      val word64ToIntInf = Word64.toLargeInt


      val intInfToWord8 = Word8.fromLargeInt
      val intInfToWord32 = Word32.fromLargeInt
      val intInfToWord64 = Word64.fromLargeInt


      fun word32ToBytesB w =
         let
            val a = Word8Array.array (4, 0w0)
         in
            PackWord32Big.update (a, 0, Word32.toLargeWord w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end

      fun word32ToBytesL w =
         let
            val a = Word8Array.array (4, 0w0)
         in
            PackWord32Little.update (a, 0, Word32.toLargeWord w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end

      fun word64ToBytesB w =
         let
            val a = Word8Array.array (8, 0w0)
         in
            PackWord32Big.update (a, 0, Word32.toLarge (word64ToWord32 (Word64.>> (w, 0w32))));
            PackWord32Big.update (a, 1, Word32.toLarge (word64ToWord32 w));
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end

      fun word64ToBytesL w =
         let
            val a = Word8Array.array (8, 0w0)
         in
            PackWord32Little.update (a, 1, Word32.toLarge (word64ToWord32 (Word64.>> (w, 0w32))));
            PackWord32Little.update (a, 0, Word32.toLarge (word64ToWord32 w));
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end


      exception ConvertWord

      fun bytesToWord32B s =
         if Bytestring.size s <> 4 then
            raise ConvertWord
         else
            Word32.fromLarge (PackWord32Big.subVec (s, 0))

      fun bytesToWord32SB s = bytesToWord32B (Bytesubstring.string s)

      fun bytesToWord32L s =
         if Bytestring.size s <> 4 then
            raise ConvertWord
         else
            Word32.fromLarge (PackWord32Little.subVec (s, 0))

      fun bytesToWord32SL s = bytesToWord32L (Bytesubstring.string s)

      fun bytesToWord64B s =
         if Bytestring.size s <> 8 then
            raise ConvertWord
         else
            Word64.orb (Word64.<< (word32ToWord64 (Word32.fromLarge (PackWord32Big.subVec (s, 0))), 0w32),
                        word32ToWord64 (Word32.fromLarge (PackWord32Big.subVec (s, 1))))

      fun bytesToWord64SB s = bytesToWord64B (Bytesubstring.string s)

      fun bytesToWord64L s =
         if Bytestring.size s <> 8 then
            raise ConvertWord
         else
            Word64.orb (Word64.<< (word32ToWord64 (Word32.fromLarge (PackWord32Little.subVec (s, 1))), 0w32),
                        word32ToWord64 (Word32.fromLarge (PackWord32Little.subVec (s, 0))))

      fun bytesToWord64SL s = bytesToWord64L (Bytesubstring.string s)



      (* This stuff depends on the size of Word/LargeWord. *)

      val wordToWord8 = Word8.fromLarge o Word.toLarge
      val wordToWord32 = Word32.fromLarge o Word.toLarge
      val wordToWord32X = Word32.fromLarge o Word.toLargeX
      val wordToWord64 = Word64.fromLarge o Word.toLarge
      val wordToWord64X = Word64.fromLarge o Word.toLargeX
      val wordToIntInf = Word.toLargeInt
      val wordToBytesB = word32ToBytesB o wordToWord32
      val wordToBytesL = word32ToBytesL o wordToWord32

      val word8ToWord = Word.fromLarge o Word8.toLarge
      val word8ToWordX = Word.fromLarge o Word8.toLargeX
      val word32ToWord = Word.fromLarge o Word32.toLarge
      val word32ToWordX = Word.fromLarge o Word32.toLargeX
      val word64ToWord = Word.fromLarge o Word64.toLarge
      val word64ToWordX = Word.fromLarge o Word64.toLargeX
      val intInfToWord = Word.fromLargeInt

      fun bytesToWordB s =
         if Bytestring.size s <> 4 orelse Word8.> (Bytestring.sub (s, 0), 0wx7f) then
            raise ConvertWord
         else
            Word.fromLarge (PackWord32Big.subVec (s, 0))

      val bytesToWordSB = bytesToWordB o Bytesubstring.string

      fun bytesToWordL s =
         if Bytestring.size s <> 4 orelse Word8.> (Bytestring.sub (s, 3), 0wx7f) then
            raise ConvertWord
         else
            Word.fromLarge (PackWord32Little.subVec (s, 0))

      val bytesToWordSL = bytesToWordL o Bytesubstring.string

      val wordLgToWord = Word.fromLarge
      val wordLgToWord8 = Word8.fromLarge
      fun wordLgToWord32 w = Word32.fromLarge w
      fun wordLgToWord32X w = Word32.fromLarge w
      val wordLgToWord64 = word32ToWord64 o Word32.fromLarge
      val wordLgToWord64X = word32ToWord64X o Word32.fromLarge
      val wordLgToIntInf = LargeWord.toLargeInt
      val wordLgToBytesB = word32ToBytesB o Word32.fromLarge
      val wordLgToBytesL = word32ToBytesL o Word32.fromLarge

      val wordToWordLg = Word.toLarge
      val word8ToWordLg = Word8.toLarge
      val word8ToWordLgX = Word8.toLargeX
      val word32ToWordLg = Word32.toLarge
      val word32ToWordLgX = Word32.toLargeX
      val word64ToWordLg = Word32.toLarge o word64ToWord32
      val word64ToWordLgX = Word32.toLarge o word64ToWord32
      val intInfToWordLg = LargeWord.fromLargeInt
      val bytesToWordLgB = Word32.toLarge o bytesToWord32B
      val bytesToWordLgSB = Word32.toLarge o bytesToWord32SB
      val bytesToWordLgL = Word32.toLarge o bytesToWord32L
      val bytesToWordLgSL = Word32.toLarge o bytesToWord32SL

   end

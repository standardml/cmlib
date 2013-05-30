
structure ConvertWord : CONVERT_WORD =
   struct

      type word = Word.word
      type wordlg = Word64.word
      type word8 = Word8.word
      type word31 = Word31.word
      type word32 = Word32.word
      type word64 = Word64.word


      (* This stuff all depends on the size of LargeWord. *)

      fun word8ToWord31 w = Word31.fromLarge (Word8.toLarge w)
      fun word8ToWord31X w = Word31.fromLarge (Word8.toLargeX w)
      fun word8ToWord32 w = Word32.fromLarge (Word8.toLarge w)
      fun word8ToWord32X w = Word32.fromLarge (Word8.toLargeX w)
      val word8ToWord64 = Word8.toLarge
      val word8ToWord64X = Word8.toLargeX
      val word8ToIntInf = Word8.toLargeInt

      fun word31ToWord8 w = Word8.fromLarge (Word31.toLarge w)
      fun word31ToWord32 w = Word32.fromLarge (Word31.toLarge w)
      fun word31ToWord32X w = Word32.fromLarge (Word31.toLargeX w)
      val word31ToWord64 = Word31.toLarge
      val word31ToWord64X = Word31.toLargeX
      val word31ToIntInf = Word31.toLargeInt

      fun word32ToWord8 w = Word8.fromLarge (Word32.toLarge w)
      fun word32ToWord31 w = Word31.fromLarge (Word32.toLarge w)
      val word32ToWord64 = Word32.toLarge
      val word32ToWord64X = Word32.toLargeX
      val word32ToIntInf = Word32.toLargeInt

      val word64ToWord8 = Word8.fromLarge
      val word64ToWord31 = Word31.fromLarge
      val word64ToWord32 = Word32.fromLarge
      val word64ToIntInf = Word64.toLargeInt

      val intInfToWord8 = Word8.fromLargeInt
      val intInfToWord31 = Word31.fromLargeInt
      val intInfToWord32 = Word32.fromLargeInt
      val intInfToWord64 = Word64.fromLargeInt


      fun word32ToBytesB w =
         let
            val a = Word8Array.array (4, 0w0)
         in
            PackWord32Big.update (a, 0, word32ToWord64 w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end

      fun word32ToBytesL w =
         let
            val a = Word8Array.array (4, 0w0)
         in
            PackWord32Little.update (a, 0, word32ToWord64 w);
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
            PackWord64Big.update (a, 0, w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end

      fun word64ToBytesL w =
         let
            val a = Word8Array.array (8, 0w0)
         in
            PackWord64Little.update (a, 0, w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end


      exception ConvertWord

      fun bytesToWord31B s =
         if Bytestring.size s <> 4 orelse Word8.> (Bytestring.sub (s, 0), 0wx7f) then
            raise ConvertWord
         else
            word64ToWord31 (PackWord32Big.subVec (Bytestring.toWord8Vector s, 0))

      fun bytesToWord31SB s = bytesToWord31B (Bytesubstring.string s)

      fun bytesToWord31L s =
         if Bytestring.size s <> 4 orelse Word8.> (Bytestring.sub (s, 3), 0wx7f) then
            raise ConvertWord
         else
            word64ToWord31 (PackWord32Little.subVec (Bytestring.toWord8Vector s, 0))

      fun bytesToWord31SL s = bytesToWord31L (Bytesubstring.string s)

      fun bytesToWord32B s =
         if Bytestring.size s <> 4 then
            raise ConvertWord
         else
            word64ToWord32 (PackWord32Big.subVec (Bytestring.toWord8Vector s, 0))

      fun bytesToWord32SB s = bytesToWord32B (Bytesubstring.string s)

      fun bytesToWord32L s =
         if Bytestring.size s <> 4 then
            raise ConvertWord
         else
            word64ToWord32 (PackWord32Little.subVec (Bytestring.toWord8Vector s, 0))

      fun bytesToWord32SL s = bytesToWord32L (Bytesubstring.string s)

      fun bytesToWord64B s =
         if Bytestring.size s <> 8 then
            raise ConvertWord
         else
            PackWord64Big.subVec (Bytestring.toWord8Vector s, 0)

      fun bytesToWord64SB s = bytesToWord64B (Bytesubstring.string s)

      fun bytesToWord64L s =
         if Bytestring.size s <> 8 then
            raise ConvertWord
         else
            PackWord64Little.subVec (Bytestring.toWord8Vector s, 0)

      fun bytesToWord64SL s = bytesToWord64L (Bytesubstring.string s)


      (* This stuff depends on the size of Word/LargeWord. *)

      val wordToWord8 = word32ToWord8
      val wordToWord31 = word32ToWord31
      fun wordToWord32 w = w
      fun wordToWord32X w = w
      val wordToWord64 = word32ToWord64
      val wordToWord64X = word32ToWord64X
      val wordToIntInf = word32ToIntInf
      val wordToBytesB = word32ToBytesB
      val wordToBytesL = word32ToBytesL

      val word8ToWord = word8ToWord32
      val word8ToWordX = word8ToWord32X
      val word31ToWord = word31ToWord32
      val word31ToWordX = word31ToWord32X
      fun word32ToWord w = w
      fun word32ToWordX w = w
      val word64ToWord = word64ToWord32
      val word64ToWordX = word64ToWord32
      val intInfToWord = intInfToWord32
      val bytesToWordB = bytesToWord32B
      val bytesToWordSB = bytesToWord32SB
      val bytesToWordL = bytesToWord32L
      val bytesToWordSL = bytesToWord32SL

      val wordLgToWord = Word.fromLarge
      val wordLgToWord8 = Word8.fromLarge
      val wordLgToWord31 = Word31.fromLarge
      val wordLgToWord32 = Word32.fromLarge
      val wordLgToWord32X = Word32.fromLarge
      fun wordLgToWord64 w = w
      fun wordLgToWord64X w = w
      val wordLgToIntInf = LargeWord.toLargeInt
      val wordLgToBytesB = word64ToBytesB
      val wordLgToBytesL = word64ToBytesL

      val wordToWordLg = Word.toLarge
      val word8ToWordLg = Word8.toLarge
      val word8ToWordLgX = Word8.toLargeX
      val word31ToWordLg = Word31.toLarge
      val word31ToWordLgX = Word31.toLargeX
      val word32ToWordLg = Word32.toLarge
      val word32ToWordLgX = Word32.toLargeX
      fun word64ToWordLg w = w
      fun word64ToWordLgX w = w
      val intInfToWordLg = LargeWord.fromLargeInt
      val bytesToWordLgB = bytesToWord64B
      val bytesToWordLgSB = bytesToWord64SB
      val bytesToWordLgL = bytesToWord64L
      val bytesToWordLgSL = bytesToWord64SL

   end

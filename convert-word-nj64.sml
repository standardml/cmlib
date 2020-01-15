
(* Abandoning backward compatibility to 32-bit SML/NJ. *)

structure ConvertWord : CONVERT_WORD =
   struct

      type word = Word.word
      type wordlg = LargeWord.word
      type word8 = Word8.word
      type word32 = Word32.word
      type word64 = Word64.word

      type word31 = unit

      fun word8ToWord32 w = Word32.fromLarge (Word8.toLarge w)
      fun word8ToWord32X w = Word32.fromLarge (Word8.toLargeX w)
      fun word8ToWord64 w = Word64.fromLarge (Word8.toLarge w)
      fun word8ToWord64X w = Word64.fromLarge (Word8.toLargeX w)
      fun word8ToIntInf w = Word8.toLargeInt w

      fun word32ToWord8 w = Word8.fromLarge (Word32.toLarge w)
      fun word32ToWord64 w = Word64.fromLarge (Word32.toLarge w)
      fun word32ToWord64X w = Word64.fromLarge (Word32.toLargeX w)
      val word32ToIntInf = Word32.toLargeInt

      fun word64ToWord8 w = Word8.fromLarge (Word64.toLarge w)
      fun word64ToWord32 w = Word32.fromLarge (Word64.toLarge w)
      val word64ToIntInf = Word64.toLargeInt

      val intInfToWord8 = Word8.fromLargeInt
      val intInfToWord32 = Word32.fromLargeInt
      val intInfToWord64 = Word64.fromLargeInt


      (* Ought to rewrite these using PackWord64<Big/Little>. *)

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



      (* This stuff depends on the size of LargeWord. *)

      fun wordToWord8 w = Word8.fromLarge (Word.toLarge w)
      fun wordToWord32 w = Word32.fromLarge (Word.toLarge w)
      fun wordToWord32X w = Word32.fromLarge (Word.toLargeX w)
      fun wordToWord64 w = Word64.fromLarge (Word.toLarge w)
      fun wordToWord64X w = Word64.fromLarge (Word.toLargeX w)
      val wordToIntInf = Word.toLargeInt
      fun wordToBytesB w = word32ToBytesB (wordToWord32 w)
      fun wordToBytesL w = word32ToBytesL (wordToWord32 w)

      fun word8ToWord w = Word.fromLarge (Word8.toLarge w)
      fun word8ToWordX w = Word.fromLarge (Word8.toLargeX w)
      fun word32ToWord w = Word.fromLarge (Word32.toLarge w)
      fun word32ToWordX w = Word.fromLarge (Word32.toLargeX w)
      fun word64ToWord w = Word.fromLarge (Word64.toLarge w)
      fun word64ToWordX w = Word.fromLarge (Word64.toLargeX w)
      val intInfToWord = Word.fromLargeInt
      fun bytesToWordB w = word32ToWord (bytesToWord32B w)
      fun bytesToWordSB w = word32ToWord (bytesToWord32SB w)
      fun bytesToWordL w = word32ToWord (bytesToWord32L w)
      fun bytesToWordSL w = word32ToWord (bytesToWord32SL w)

      val wordLgToWord = Word.fromLarge
      val wordLgToWord8 = Word8.fromLarge
      fun wordLgToWord32 w = Word32.fromLarge w
      fun wordLgToWord32X w = Word32.fromLarge w
      fun wordLgToWord64 w = w
      fun wordLgToWord64X w = w
      val wordLgToIntInf = LargeWord.toLargeInt
      fun wordLgToBytesB w = word64ToBytesB w
      fun wordLgToBytesL w = word64ToBytesL w

      val wordToWordLg = Word.toLarge
      val word8ToWordLg = Word8.toLarge
      val word8ToWordLgX = Word8.toLargeX
      val word32ToWordLg = Word32.toLarge
      val word32ToWordLgX = Word32.toLargeX
      fun word64ToWordLg w = w
      fun word64ToWordLgX w = w
      val intInfToWordLg = LargeWord.fromLargeInt
      fun bytesToWordLgB w = bytesToWord64B w
      fun bytesToWordLgSB w = bytesToWord64SB w
      fun bytesToWordLgL w = bytesToWord64L w
      fun bytesToWordLgSL w = bytesToWord64SL w

      fun wordToWord31 w = ()
      fun wordLgToWord31 w = ()
      fun word8ToWord31 w = ()
      fun word8ToWord31X w = ()
      fun word32ToWord31 w = ()
      fun word64ToWord31 w = ()
      fun intInfToWord31 _ = ()

      fun bytesToWord31B s = ()
      fun bytesToWord31SB s = ()
      fun bytesToWord31L s = ()
      fun bytesToWord31SL s = ()

      fun word31ToWord w = 0w0 : Word.word
      fun word31ToWordX w = 0w0 : Word.word
      fun word31ToWordLg w = 0w0 : LargeWord.word
      fun word31ToWordLgX w = 0w0 : LargeWord.word
      fun word31ToWord8 w = 0w0 : Word8.word
      fun word31ToWord32 w = 0w0 : Word32.word
      fun word31ToWord32X w = 0w0 : Word32.word
      fun word31ToWord64 w = 0w0 : Word64.word
      fun word31ToWord64X w = 0w0 : Word64.word
      fun word31ToIntInf w = 0 : IntInf.int

      fun word31ToBytesB w = Bytestring.null
      fun word31ToBytesL w = Bytestring.null

   end

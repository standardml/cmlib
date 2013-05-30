
signature CONVERT_WORD =
   sig

      (* required words *)
      type word = Word.word              (* must be at least 31 bits *)
      type wordlg = LargeWord.word       (* must be at least 31 bits *)
      type word8 = Word8.word
      type word32 = Word32.word          (* optional in the SML Basis, but required by CMLIB *)
      
      (* optional words, set to unit if not supported *)
      type word31
      type word64

      val wordToWordLg : word -> wordlg
      val wordToWord8 : word -> word8
      val wordToWord31 : word -> word31
      val wordToWord32 : word -> word32
      val wordToWord32X : word -> word32
      val wordToWord64 : word -> word64
      val wordToWord64X : word -> word64
      val wordToBytesB : word -> Bytestring.string
      val wordToBytesL : word -> Bytestring.string
      val wordToIntInf : word -> IntInf.int

      val wordLgToWord : wordlg -> word
      val wordLgToWord8 : wordlg -> word8
      val wordLgToWord31 : wordlg -> word31
      val wordLgToWord32 : wordlg -> word32
      val wordLgToWord32X : wordlg -> word32
      val wordLgToWord64 : wordlg -> word64
      val wordLgToWord64X : wordlg -> word64
      val wordLgToBytesB : wordlg -> Bytestring.string
      val wordLgToBytesL : wordlg -> Bytestring.string
      val wordLgToIntInf : wordlg -> IntInf.int

      val word8ToWord : word8 -> word
      val word8ToWordX : word8 -> word
      val word8ToWordLg : word8 -> wordlg
      val word8ToWordLgX : word8 -> wordlg
      val word8ToWord31 : word8 -> word31
      val word8ToWord31X : word8 -> word31
      val word8ToWord32 : word8 -> word32
      val word8ToWord32X : word8 -> word32
      val word8ToWord64 : word8 -> word64
      val word8ToWord64X : word8 -> word64
      val word8ToIntInf : word8 -> IntInf.int

      val word31ToWord : word31 -> word
      val word31ToWordX : word31 -> word
      val word31ToWordLg : word31 -> wordlg
      val word31ToWordLgX : word31 -> wordlg
      val word31ToWord8 : word31 -> word8
      val word31ToWord32 : word31 -> word32
      val word31ToWord32X : word31 -> word32
      val word31ToWord64 : word31 -> word64
      val word31ToWord64X : word31 -> word64
      val word31ToBytesB : word31 -> Bytestring.string
      val word31ToBytesL : word31 -> Bytestring.string
      val word31ToIntInf : word31 -> IntInf.int

      val word32ToWord : word32 -> word
      val word32ToWordX : word32 -> word
      val word32ToWordLg : word32 -> wordlg
      val word32ToWordLgX : word32 -> wordlg
      val word32ToWord8 : word32 -> word8
      val word32ToWord31 : word32 -> word31
      val word32ToWord64 : word32 -> word64
      val word32ToWord64X : word32 -> word64
      val word32ToBytesB : word32 -> Bytestring.string
      val word32ToBytesL : word32 -> Bytestring.string
      val word32ToIntInf : word32 -> IntInf.int

      val word64ToWord : word64 -> word
      val word64ToWordX : word64 -> word
      val word64ToWordLg : word64 -> wordlg
      val word64ToWordLgX : word64 -> wordlg
      val word64ToWord8 : word64 -> word8
      val word64ToWord31 : word64 -> word31
      val word64ToWord32 : word64 -> word32
      val word64ToBytesB : word64 -> Bytestring.string
      val word64ToBytesL : word64 -> Bytestring.string
      val word64ToIntInf : word64 -> IntInf.int

      val intInfToWord : IntInf.int -> word
      val intInfToWordLg : IntInf.int -> wordlg
      val intInfToWord8 : IntInf.int -> word8
      val intInfToWord31 : IntInf.int -> word31
      val intInfToWord32 : IntInf.int -> word32
      val intInfToWord64 : IntInf.int -> word64

      exception ConvertWord
      val bytesToWordB : Bytestring.string -> word
      val bytesToWordL : Bytestring.string -> word
      val bytesToWordLgB : Bytestring.string -> wordlg
      val bytesToWordLgL : Bytestring.string -> wordlg
      val bytesToWord31B : Bytestring.string -> word31
      val bytesToWord31L : Bytestring.string -> word31
      val bytesToWord32B : Bytestring.string -> word32
      val bytesToWord32L : Bytestring.string -> word32
      val bytesToWord64B : Bytestring.string -> word64
      val bytesToWord64L : Bytestring.string -> word64

      val bytesToWordSB : Bytesubstring.substring -> word
      val bytesToWordSL : Bytesubstring.substring -> word
      val bytesToWordLgSB : Bytesubstring.substring -> wordlg
      val bytesToWordLgSL : Bytesubstring.substring -> wordlg
      val bytesToWord31SB : Bytesubstring.substring -> word31
      val bytesToWord31SL : Bytesubstring.substring -> word31
      val bytesToWord32SB : Bytesubstring.substring -> word32
      val bytesToWord32SL : Bytesubstring.substring -> word32
      val bytesToWord64SB : Bytesubstring.substring -> word64
      val bytesToWord64SL : Bytesubstring.substring -> word64

   end

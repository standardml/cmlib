
signature CONVERT_WORD =
   sig

      (* required words *)
      type word = Word.word
      type word8 = Word8.word
      type word32 = Word32.word  (* optional in the SML Basis, but required by CMLIB *)
      
      (* optional words, set to unit if not supported *)
      type word31
      type word64

      val wordToWord8 : word -> word8
      val wordToWord31 : word -> word31
      val wordToWord32 : word -> word32
      val wordToWord32X : word -> word32
      val wordToWord64 : word -> word64
      val wordToWord64X : word -> word64
      val wordToBytesB : word -> Bytestring.string
      val wordToBytesL : word -> Bytestring.string
      val wordToIntInf : word -> IntInf.int

      val word8ToWord : word8 -> word
      val word8ToWordX : word8 -> word
      val word8ToWord31 : word8 -> word31
      val word8ToWord31X : word8 -> word31
      val word8ToWord32 : word8 -> word32
      val word8ToWord32X : word8 -> word32
      val word8ToWord64 : word8 -> word64
      val word8ToWord64X : word8 -> word64
      val word8ToIntInf : word8 -> IntInf.int

      val word31ToWord : word31 -> word
      val word31ToWordX : word31 -> word
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
      val word32ToWord8 : word32 -> word8
      val word32ToWord31 : word32 -> word31
      val word32ToWord64 : word32 -> word64
      val word32ToWord64X : word32 -> word64
      val word32ToBytesB : word32 -> Bytestring.string
      val word32ToBytesL : word32 -> Bytestring.string
      val word32ToIntInf : word32 -> IntInf.int

      val word64ToWord : word64 -> word
      val word64ToWordX : word64 -> word
      val word64ToWord8 : word64 -> word8
      val word64ToWord31 : word64 -> word31
      val word64ToWord32 : word64 -> word32
      val word64ToBytesB : word64 -> Bytestring.string
      val word64ToBytesL : word64 -> Bytestring.string
      val word64ToIntInf : word64 -> IntInf.int

      val intInfToWord : IntInf.int -> word
      val intInfToWord8 : IntInf.int -> word8
      val intInfToWord31 : IntInf.int -> word31
      val intInfToWord32 : IntInf.int -> word32
      val intInfToWord64 : IntInf.int -> word64

      exception ConvertWord
      val bytesToWordB : Bytestring.string -> word
      val bytesToWordL : Bytestring.string -> word
      val bytesToWord31B : Bytestring.string -> word31
      val bytesToWord31L : Bytestring.string -> word31
      val bytesToWord32B : Bytestring.string -> word32
      val bytesToWord32L : Bytestring.string -> word32
      val bytesToWord64B : Bytestring.string -> word64
      val bytesToWord64L : Bytestring.string -> word64

   end

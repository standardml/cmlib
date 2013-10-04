
structure TextIOUtil
   :>
   IO_UTIL
   where type outstream = TextIO.outstream
   =
   struct

      type outstream = TextIO.outstream

      val nullOut =
         TextIO.mkOutstream
         (TextIO.StreamIO.mkOutstream
             (TextPrimIO.WR
                 { 
                 name = "null",
                 chunkSize = 1,
                 writeVec = SOME (fn vec => CharVectorSlice.length vec),
                 writeArr = SOME (fn arr => CharArraySlice.length arr),
                 writeVecNB = SOME (fn vec => SOME (CharVectorSlice.length vec)),
                 writeArrNB = SOME (fn arr => SOME (CharArraySlice.length arr)),
                 block = SOME (fn () => ()),
                 canOutput = SOME (fn () => true),
                 getPos = SOME (fn () => 0),
                 setPos = SOME (fn _ => ()),
                 endPos = SOME (fn () => 0),
                 verifyPos = SOME (fn () => 0),
                 close = (fn () => ()),
                 ioDesc = NONE
                 },
              IO.NO_BUF))

   end


structure BinIOUtil
   :>
   IO_UTIL
   where type outstream = BinIO.outstream
   =
   struct

      type outstream = BinIO.outstream

      val nullOut =
         BinIO.mkOutstream
         (BinIO.StreamIO.mkOutstream
             (BinPrimIO.WR
                 { 
                 name = "null",
                 chunkSize = 1,
                 writeVec = SOME (fn vec => Word8VectorSlice.length vec),
                 writeArr = SOME (fn arr => Word8ArraySlice.length arr),
                 writeVecNB = SOME (fn vec => SOME (Word8VectorSlice.length vec)),
                 writeArrNB = SOME (fn arr => SOME (Word8ArraySlice.length arr)),
                 block = SOME (fn () => ()),
                 canOutput = SOME (fn () => true),
                 getPos = SOME (fn () => 0),
                 setPos = SOME (fn _ => ()),
                 endPos = SOME (fn () => 0),
                 verifyPos = SOME (fn () => 0),
                 close = (fn () => ()),
                 ioDesc = NONE
                 },
              IO.NO_BUF))

   end

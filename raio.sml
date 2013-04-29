
structure RAIO :> RAIO =
   struct

      structure P = BinPrimIO

      type vector = Word8Vector.vector
      type elem = Word8.word

      type pos = BinPrimIO.pos
      type instream = P.reader
      type outstream = P.writer

      fun fromReader (s as P.RD { name, readVec, getPos, setPos, ... }) =
         if isSome readVec andalso isSome getPos andalso isSome setPos then
            s
         else
            raise (IO.Io { name=name, function="fromReader", cause=IO.RandomAccessNotSupported })

      fun fromWriter (s as P.WR { name, writeVec, getPos, setPos, ... }) =
         if isSome writeVec andalso isSome getPos andalso isSome setPos then
            s
         else
            raise (IO.Io { name=name, function="fromWriter", cause=IO.RandomAccessNotSupported })

      fun toReader s = s
      fun toWriter s = s

      fun fromInstream s = fromReader (#1 (BinIO.StreamIO.getReader (BinIO.getInstream s)))

      fun fromOutstream s = fromWriter (#1 (BinIO.StreamIO.getWriter (BinIO.getOutstream s)))

      fun posIn (P.RD { getPos, ... }) = valOf getPos ()

      fun seekIn (P.RD { setPos, ... }, pos) = valOf setPos pos
      
      fun closeIn (P.RD { close, ... }) = close ()

      fun input (P.RD { chunkSize, readVec, ... }) = valOf readVec chunkSize

      fun input1 (P.RD { readVec, ... }) =
         let
            val vec = valOf readVec 1
         in
            if Word8Vector.length vec = 1 then
               SOME (Word8Vector.sub (vec, 0))
            else
               NONE
         end

      fun inputN (P.RD { readVec, ... }, n) = valOf readVec n



      fun posOut (P.WR { getPos, ... }) = valOf getPos ()

      fun seekOut (P.WR { setPos, ... }, pos) = valOf setPos pos

      fun closeOut (P.WR { close, ... }) = close ()

      fun output (P.WR { writeVec, ... }, vec) = (valOf writeVec (Word8VectorSlice.full vec); ())
      
      fun output1 (P.WR { writeVec, ... }, elem) =
         (valOf writeVec (Word8VectorSlice.full (Word8Vector.fromList [elem])); ())
      


      exception DummyStream
      fun dummy f = SOME (fn _ => raise IO.Io { name="dummy", function=f, cause=DummyStream })

      val dummyIn =
         P.RD
         { name="dummy",
           chunkSize = 1,
           readVec=dummy "readVec",
           readArr=NONE,
           readVecNB=NONE,
           readArrNB=NONE,
           block=NONE,
           canInput=NONE,
           avail=(fn () => NONE),
           getPos=dummy "getPos",
           setPos=dummy "setPos",
           endPos=NONE,
           verifyPos=NONE,
           close=(fn () => ()),
           ioDesc=NONE }
     
      val dummyOut =
         BinPrimIO.WR
         { name="dummy",
           chunkSize=1,
           writeVec=dummy "writeVec",
           writeArr=NONE,
           writeVecNB=NONE,
           writeArrNB=NONE,
           block=NONE,
           canOutput=NONE,
           getPos=dummy "getPos",
           setPos=dummy "setPos",
           endPos=NONE,
           verifyPos=NONE,
           close=(fn () => ()),
           ioDesc=NONE}

   end

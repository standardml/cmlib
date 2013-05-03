
structure IOUtil :> IO_UTIL =
   struct

      exception SeekImpossible

      fun seekIn (ins, pos) =
         let
            val (reader, _) = BinIO.StreamIO.getReader (BinIO.getInstream ins)
            val BinPrimIO.RD {setPos, ...} = reader
         in
            (case setPos of
                NONE =>
                   raise SeekImpossible
              | SOME f =>
                   f pos);

            BinIO.setInstream (ins, BinIO.StreamIO.mkInstream (reader, Bytestring.null))
         end

      fun seekOut (outs, pos) =
         let
            val (writer, mode) = BinIO.StreamIO.getWriter (BinIO.getOutstream outs)
            val BinPrimIO.WR {setPos, ...} = writer
         in
            (case setPos of
                NONE =>
                   raise SeekImpossible
              | SOME f =>
                   f pos);

            BinIO.setOutstream (outs, BinIO.StreamIO.mkOutstream (writer, mode))
         end


      exception DummyStream

      val dummyIn =
         BinIO.mkInstream
         (BinIO.StreamIO.mkInstream
             (BinPrimIO.RD
              { name="dummy",
                chunkSize = 1,
                readVec=SOME (fn _ => raise IO.Io { name="dummy", function="readVec", cause=DummyStream }),
                readArr=NONE,
                readVecNB=NONE,
                readArrNB=NONE,
                block=NONE,
                canInput=NONE,
                avail=(fn () => NONE),
                getPos=SOME (fn () => 0),
                setPos=SOME (fn _ => ()),
                endPos=NONE,
                verifyPos=NONE,
                close=(fn () => ()),
                ioDesc=NONE },
              Bytestring.null))

      val dummyOut =
         BinIO.mkOutstream
         (BinIO.StreamIO.mkOutstream
             (BinPrimIO.WR
              { name="dummy",
                chunkSize=1,
                writeVec=SOME (fn _ => raise IO.Io { name="dummy", function="writeVec", cause=DummyStream }),
                writeArr=NONE,
                writeVecNB=NONE,
                writeArrNB=NONE,
                block=NONE,
                canOutput=NONE,
                getPos=SOME (fn () => 0),
                setPos=SOME (fn _ => ()),
                endPos=NONE,
                verifyPos=NONE,
                close=(fn () => ()),
                ioDesc=NONE},
              IO.NO_BUF))

   end

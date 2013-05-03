
signature IO_UTIL =
   sig

      exception SeekImpossible
      val seekIn : BinIO.instream * BinPrimIO.pos -> unit
      val seekOut : BinIO.outstream * BinPrimIO.pos -> unit

      exception DummyStream
      val dummyIn : BinIO.instream
      val dummyOut : BinIO.outstream

   end

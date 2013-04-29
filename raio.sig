
(* Random-access I/O *)

signature RAIO =
   sig

      type vector = Word8Vector.vector
      type elem = Word8.word

      type pos = BinPrimIO.pos
      type instream
      type outstream

      val seekIn : instream * pos -> unit
      val posIn : instream -> pos
      val closeIn : instream -> unit
      val input : instream -> vector
      val input1 : instream -> elem option
      val inputN : instream * int -> vector

      val seekOut : outstream * pos -> unit
      val posOut : outstream -> pos
      val closeOut : outstream -> unit
      val output : outstream * vector -> unit
      val output1 : outstream * elem -> unit

      val fromInstream : BinIO.instream -> instream
      val fromReader : BinPrimIO.reader -> instream
      val toReader : instream -> BinPrimIO.reader
      val fromOutstream : BinIO.outstream -> outstream
      val fromWriter : BinPrimIO.writer -> outstream
      val toWriter : outstream -> BinPrimIO.writer

      exception DummyStream
      val dummyIn : instream
      val dummyOut : outstream

   end

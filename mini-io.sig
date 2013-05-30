
signature MINI_IO =
   sig

      type vector
      type elem

      type instream
      type outstream

      val input1 : instream -> elem option
      val inputN : instream * int -> vector
      val closeIn : instream -> unit
      val endOfStream : instream -> bool

      val output : outstream * vector -> unit
      val output1 : outstream * elem -> unit
      val flushOut : outstream -> unit
      val closeOut : outstream -> unit

   end

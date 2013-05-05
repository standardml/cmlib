
signature SEEK_IO =
   sig

      type instream
      type outstream
      type pos

      exception SeekImpossible
      val seekIn : instream * pos -> unit
      val seekOut : outstream * pos -> unit

   end

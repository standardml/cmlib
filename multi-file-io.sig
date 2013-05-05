
signature MULTI_FILE_IO =
   sig

      include MINI_IO

      val openIn : string -> instream
      val openAppend : string -> outstream
      val exists : string -> bool

      type pos

      val getPosIn : instream -> pos
      val getPosOut : outstream -> pos

      (* Can seek outside current file contents, but only within the current piece. *)
      structure SeekIO : SEEK_IO
                         where type instream = instream
                         where type outstream = outstream
                         where type pos = pos

   end

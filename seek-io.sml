
functor SeekIOFun (structure ImpIO : IMPERATIVE_IO
                   structure PrimIO : PRIM_IO
                   val nullVector : ImpIO.vector
                   sharing type ImpIO.vector = PrimIO.vector
                   sharing type ImpIO.elem = PrimIO.elem
                   sharing type ImpIO.StreamIO.reader = PrimIO.reader
                   sharing type ImpIO.StreamIO.writer = PrimIO.writer)
   :>
   SEEK_IO
   where type instream = ImpIO.instream
   where type outstream = ImpIO.outstream
   where type pos = PrimIO.pos
   =
   struct

      type instream = ImpIO.instream
      type outstream = ImpIO.outstream
      type pos = PrimIO.pos

      exception SeekImpossible

      fun seekIn (ins, pos) =
         let
            val (reader, _) = ImpIO.StreamIO.getReader (ImpIO.getInstream ins)
            val PrimIO.RD {setPos, ...} = reader
         in
            (case setPos of
                NONE =>
                   raise SeekImpossible
              | SOME f =>
                   f pos);

            ImpIO.setInstream (ins, ImpIO.StreamIO.mkInstream (reader, nullVector))
         end

      fun seekOut (outs, pos) =
         let
            val (writer, mode) = ImpIO.StreamIO.getWriter (ImpIO.getOutstream outs)
            val PrimIO.WR {setPos, ...} = writer
         in
            (case setPos of
                NONE =>
                   raise SeekImpossible
              | SOME f =>
                   f pos);

            ImpIO.setOutstream (outs, ImpIO.StreamIO.mkOutstream (writer, mode))
         end

   end


structure BinSeekIO =
   SeekIOFun 
   (structure ImpIO = BinIO
    structure PrimIO = BinPrimIO
    val nullVector = Bytestring.null)

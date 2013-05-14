
functor MultiFileIOFun (structure ImpIO 
                           : sig
                                include MINI_IO
                                val openIn : string -> instream
                                val openOut : string -> outstream
                                val openAppend : string -> outstream
                             end
                        structure String
                           : sig
                                type string
                                val size : string -> int
                                val ^ : string * string -> string
                                val null : string
                                val substring : string * int * int -> string
                             end
                        structure SeekIO : SEEK_IO where type pos = Position.int
                        val fileSize : string -> Position.int
                        val pieceSize : int
                        structure Integer : INTEGER
                        sharing type ImpIO.vector = String.string
                        sharing type ImpIO.instream = SeekIO.instream
                        sharing type ImpIO.outstream = SeekIO.outstream)
   :>
   MULTI_FILE_IO
   where type vector = ImpIO.vector
   where type elem = ImpIO.elem
   where type pos = Integer.int
   =
   struct

      (* check that pieceSize > 0 *)
      val () =
         if pieceSize <= 0 then
            raise (Fail "MultiFileIOFun requires positive pieceSize")
         else
            ()

      structure I = Integer

      val op + = I.+
      val op - = I.-
      val op * = I.*
      val op div = I.div
      val op mod = I.mod
      val op < = I.<
      val op <= = I.<=
      val op > = I.>
      val op >= = I.>=
      val zero = I.fromInt 0
      val one = I.fromInt 1
      val two = I.fromInt 2
      val pieceSize = I.fromInt pieceSize


      type vector = ImpIO.vector
      type elem = ImpIO.elem

      type 'a multi =
         { master : string,
           closed : bool ref,
           curr : 'a ref,
           max : I.int ref,
           pos : I.int ref }

      (* Invariants:
         !curr is closed iff !closed
         !max is divisible by pieceSize
         !max-pieceSize <= !pos < !max  orelse  !closed
         all pieces up to the last one exist
      *)

           
      type instream = ImpIO.instream multi
      type outstream = ImpIO.outstream multi
      type pos = I.int


      exception FileNotFound

      
      fun fileExists filename =
         (fileSize filename; true)
         handle OS.SysErr _ => false
              | Overflow => true

      fun ImpIO_openAppend filename =
         (* Shouldn't be necessary, but not every implementation of openAppend follows the spec. *)
         if fileExists filename then
            ImpIO.openAppend filename
         else
            ImpIO.openOut filename
            
         

      fun piecename filename piece = filename ^ "." ^ I.toString piece

      
      (* piece 0 must exist already *)
      fun lastPiece filename =
         let
            (* precondition: left < right, left exists, right does not *)
            fun binloop left right =
               let
                  val middle = (left + right) div two
               in
                  if middle = left then
                     left
                  else if fileExists (piecename filename middle) then
                     binloop middle right
                  else
                     binloop left middle
               end

            (* precondition: n-1 exists *)
            fun exploop n =
               if fileExists (piecename filename (two * n - one)) then
                  exploop (two * n)
               else
                  binloop (n - one) (two * n - one)
         in
            exploop one
         end


      fun openIn filename =
         let
            val name = piecename filename zero

            val s =
               if fileExists name then
                  ImpIO.openIn name
               else
                  raise (IO.Io { name=filename, function="openIn", cause=FileNotFound })
         in
            { master = filename,
              closed = ref false,
              curr = ref s,
              max = ref pieceSize,
              pos = ref zero }
         end


      fun openAppend filename =
         let
            (* Make sure piece zero exists *)
            val name0 = piecename filename zero
            val s0 = ImpIO_openAppend name0

            (* Find the last piece *)
            val (n, name) =
               let
                  val n = lastPiece filename
                  val name = piecename filename n
               in
                  (* if the last existing piece is full, pick the next one *)
                  if I.fromInt (Position.toInt (fileSize name)) >= pieceSize then
                     (n + one, piecename filename (n + one))
                  else
                     (n, name)
               end

            val s =
               if n = zero then
                  s0
               else
                  let in
                     ImpIO.closeOut s0;
                     ImpIO_openAppend name
                  end
         in
            { master = filename,
              closed = ref false,
              curr = ref s,
              max = ref ((n + one) * pieceSize),
              pos = ref (n * pieceSize + I.fromInt (Position.toInt (fileSize name)))}
         end


      fun closeIn ({curr, closed, ...}:instream) =
         if !closed then
            ()
         else
            let in
               ImpIO.closeIn (!curr);
               closed := true
            end


      fun closeOut ({curr, closed, ...}:outstream) =
         if !closed then
            ()
         else
            let in
               ImpIO.closeOut (!curr);
               closed := true
            end


      fun exists filename = fileExists (piecename filename zero)


      fun getPosIn ({master, closed, pos, ...}:instream) =
         if !closed then
            raise (IO.Io { name=master, function="getPosIn", cause=IO.ClosedStream })
         else
            !pos
            

      fun getPosOut ({master, closed, pos, ...}:outstream) =
         if !closed then
            raise (IO.Io { name=master, function="getPosOut", cause=IO.ClosedStream })
         else
            !pos
            

      (* pos invariant may not apply *)
      fun adjustIn ({master, closed, curr, pos, max, ...}:instream) =
         let 
            val () = ImpIO.closeIn (!curr)
            val n = !pos div pieceSize
            val name = piecename master n
         in
            if fileExists name then
               let
                  val s = ImpIO.openIn name
                  val i = Position.fromInt (I.toInt (!pos mod pieceSize))
               in
                  if i = 0 then
                     ()
                  else
                     SeekIO.seekIn (s, i);
                  curr := s;
                  max := (n + one) * pieceSize;
                  true
               end
            else
               let in
                  closed := true;
                  false
               end
         end


      (* pos invariant may not apply *)
      exception AdjustOutOfRange
      fun adjustOut ({master, closed, curr, pos, max, ...}:outstream) allowExtend =
         let 
            val () = ImpIO.closeOut (!curr)
            val n = !pos div pieceSize
            val name = piecename master n

            val s =
               if fileExists name then
                  ImpIO.openAppend name
               else if allowExtend then
                  ImpIO.openOut name
               else
                  raise AdjustOutOfRange

            val i = Position.fromInt (I.toInt (!pos mod pieceSize))
         in
            SeekIO.seekOut (s, i);
            curr := s;
            max := (n + one) * pieceSize;
            true
         end
         handle
            AdjustOutOfRange =>
               let in
                  closed := true;
                  false
               end


      fun input1 (stream as {closed, curr, max, pos, ...}:instream) =
         if !closed then
            NONE
         else
            (case ImpIO.input1 (!curr) of
                res as SOME x =>
                   let in
                      pos := !pos + one;

                      if !pos >= !max then
                         (adjustIn stream; ())
                      else
                         ();

                      res
                   end
              | NONE =>
                   let in
                      ImpIO.closeIn (!curr);
                      closed := true;
                      NONE
                   end)


      fun inputN (stream as {closed, curr, max, pos, ...}:instream, sz) =
         if !closed then
            String.null
         else
            let
               val sz' = I.fromInt sz
            in
               if !pos + sz' >= !max then
                  (* input straddles the boundary *)
                  if sz' > pieceSize then
                     raise Size
                  else
                     let
                        val leftsz = I.toInt (!max - !pos)
                        val left = ImpIO.inputN (!curr, leftsz)
                        val leftszActual = String.size left
                     in
                        pos := !pos + I.fromInt leftszActual;
   
                        if Int.< (leftszActual, leftsz) then
                           let in
                              ImpIO.closeIn (!curr);
                              closed := true;
                              left
                           end
                        else
                           if (pos := !max; adjustIn stream) then
                              let
                                 val rightsz = Int.- (sz, leftsz)
                                 val right = ImpIO.inputN (!curr, rightsz)
                                 val rightszActual = String.size right
                              in
                                 pos := !pos + I.fromInt rightszActual;
      
                                 String.^ (left, right)
                              end
                           else
                              (* next file didn't exist *)
                              left
                     end
               else
                  let
                     val res = ImpIO.inputN (!curr, sz)
                  in
                     pos := !pos + sz';
                     res
                  end
            end


      fun endOfStream ({closed, curr, ...}:instream) =
         !closed  orelse  ImpIO.endOfStream (!curr)



      fun output1 (stream as {master, closed, curr, max, pos, ...}:outstream, elem) =
         if !closed then
            raise (IO.Io { name=master, function="output", cause=IO.ClosedStream })
         else
            let in
               ImpIO.output1 (!curr, elem);
               pos := !pos + one;

               if !pos >= !max then
                  (adjustOut stream true; ())
               else
                  ()
            end


      fun output (stream as {master, closed, curr, max, pos, ...}:outstream, vec) =
         if !closed then
            raise (IO.Io { name=master, function="output", cause=IO.ClosedStream })
         else
            let
               val sz = String.size vec
               val sz' = I.fromInt sz
            in
               if !pos + sz' >= !max then
                  (* output straddles the boundary *)
                  if sz' > pieceSize then
                     raise Size
                  else
                     let
                        val leftsz = I.toInt (!max - !pos)
                        val left = String.substring (vec, 0, leftsz)
                     in
                        ImpIO.output (!curr, left);
                        pos := !max;
                        (adjustOut stream true; ());
                        let
                           val rightsz = Int.- (sz, leftsz)
                           val right = String.substring (vec, leftsz, rightsz)
                        in
                           ImpIO.output (!curr, right);
                           pos := !pos + I.fromInt rightsz
                        end
                     end
               else
                  let
                     val res = ImpIO.output (!curr, vec)
                  in
                     pos := !pos + sz'
                  end
            end


      fun flushOut ({closed, curr, ...}:outstream) =
         if !closed then
            ()
         else
            ImpIO.flushOut (!curr)


      structure SeekIO =
         struct

            type instream = instream
            type outstream = outstream
            type pos = pos

            exception SeekImpossible = SeekIO.SeekImpossible
            exception OutOfRange


            fun seekIn (stream as {master, closed, curr, max, pos, ...}:instream, i) =
               if !closed then
                  raise (IO.Io { name=master, function="seekIn", cause=IO.ClosedStream })
               else if !max > i andalso i >= !max - pieceSize then
                  (* staying within same piece *)
                  let in
                     SeekIO.seekIn (!curr, Position.fromInt (I.toInt (i mod pieceSize)));
                     pos := i
                  end
               else
                  let in
                     pos := i;

                     if adjustIn stream then
                        ()
                     else
                        raise (IO.Io { name=master, function="seekIn", cause=OutOfRange })
                  end

                           
            fun seekOut (stream as {master, closed, curr, max, pos, ...}:outstream, i) =
               if !closed then
                  raise (IO.Io { name=master, function="seekOut", cause=IO.ClosedStream })
               else if !max > i andalso i >= !max - pieceSize then
                  (* staying within same piece *)
                  let in
                     SeekIO.seekOut (!curr, Position.fromInt (I.toInt (i mod pieceSize)));
                     pos := i
                  end
               else
                  let in
                     pos := i;

                     if adjustOut stream false then
                        ()
                     else
                        (* Don't allow seeking to a non-existent piece, to avoid creating holes. *)
                        raise (IO.Io { name=master, function="seekOut", cause=OutOfRange })
                  end
                           
         end

   end

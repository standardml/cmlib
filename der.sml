
(* Warning! This is only sparsely tested. *)

structure DER
   :> 
   ASN1 where type data = Bytestring.string
   =
   struct

      structure S = TreeSequence
      structure B = Bytestring
      structure BS = Bytesubstring
      structure C = ConvertIntInf

      type data = B.string


      datatype class =
         Choice of Word8.word list     (* nonempty choice row (but possibly degenerate with just one choice) *)
       | OptOpen of Word8.word list    (* nonempty sequence row consisting entirely of optional fields *)
       | OptClosed of Word8.word list  (* sequence row beginning with required fields and
                                          ending with (possibly zero) optional fields
                                       *)
       | Any                           (* single item with unknown tag *)

      
      type 'a format =
         class
         * ('a -> int * B.string S.seq)          (* encoder *)
         * (BS.substring -> 'a * BS.substring)   (* decoder *)

      datatype sum = datatype Sum.sum


      datatype decodeError =
         ILLEGAL of int
       | MISMATCH of int
       | FIXED

      exception IllegalFormat
      exception EncodeError
      exception DecodeError of decodeError

      fun error f s =
         let
            val (_, pos, _) = BS.base s
         in
            raise (DecodeError (f pos))
         end



      fun map f g (ts, out, inn) =
         let
            fun out' x = out (g x)

            fun inn' s =
               let
                  val (x, s') = inn s
               in
                  (f x, s')
               end
         in
            (ts, out', inn')
         end


      fun overlap l1 l2 =
         (* Asymptotically this is a poor way to check disjointness, but we expect these lists to be very short. *)
         List.exists (fn t1 : Word8.word => List.exists (fn t2 => t1 = t2) l2) l1

      fun member t l =
         List.exists (fn t' : Word8.word => t = t') l

      
      fun sizeOut n =
         if n < 128 then
            (1, S.singleton (B.str (Word8.fromInt n)))
         else
            let
               val n' = IntInf.fromInt n
               val bytes = (IntInf.log2 n') div 8 + 1
            in
               if bytes > 126 then
                  (* Objects this preposterously large are illegal.  Anyway, we will have
                     already overflowed the size long before now.
                  *)
                  raise EncodeError
               else
                  (bytes + 1,
                   S.cons (B.str (Word8.orb (0wx80, Word8.fromInt bytes)),
                           S.singleton (C.toBytesB n')))
            end

      fun sizeIn s =
         (case BS.getc s of
             NONE =>
                error ILLEGAL s
           | SOME (b, s') =>
                if Word8.< (b, 0w128) then
                   (Word8.toInt b, s')
                else
                   let
                      val bytes = Word8.toInt (Word8.andb (b, 0wx7f))
                   in
                      if bytes = 0 orelse bytes = 127 then
                         (* 0 indicates indefinite-length, which is illegal in DER.
                            127 bytes is too large and illegal.
                         *)
                         error ILLEGAL s
                      else
                         let
                            val (front, back) =
                               BS.splitAt (s', bytes)
                               handle Subscript => error ILLEGAL s
                         in
                            (IntInf.toInt (C.fromBytesB (BS.string front)),
                             back)
                         end
                   end)
         
      fun inhead b s =
         (case BS.getc s of
             NONE =>
                error MISMATCH s
           | SOME (b', s') =>
                if b <> b' then
                   error MISMATCH s
                else
                   sizeIn s')

      fun concat ((class1, out1, in1), (class2, out2, in2)) =
         let
            val class =
               (case (class1, class2) of
                   (OptOpen l1, OptOpen l2) =>
                      if overlap l1 l2 then
                         raise IllegalFormat
                      else
                         OptOpen (l1 @ l2)
      
                 | (OptOpen _, _) =>
                      (* optional fields must appear after all required fields *)
                      raise IllegalFormat
      
                 | (OptClosed l1, OptOpen l2) =>
                      if overlap l1 l2 then
                         raise IllegalFormat
                      else
                         OptClosed (l1 @ l2)
      
                 | (_, OptOpen l) =>
                      OptClosed l
      
                 | (OptClosed (_ :: _), _) =>
                      (* optional fields must appear after all required fields *)
                      raise IllegalFormat
                      
                 | (_, OptClosed _) =>
                      class2
      
                 | _ => OptClosed [])

            fun out (x1, x2) =
               let
                  val (sz1, seq1) = out1 x1
                  val (sz2, seq2) = out2 x2
               in
                  (sz1 + sz2,
                   S.append (seq1, seq2))
               end

            fun inn s =
               let
                  val (x1, s1) = in1 s
                  val (x2, s2) = in2 s1
               in
                  ((x1, x2), s2)
               end
         in
            (class, out, inn)
         end


      fun sequence (_, out, inn) =
         let
            fun out' x =
               let
                  val (sz, seq) = out x

                  val (szsz, szcode) = sizeOut sz
               in
                  (sz + szsz + 1,
                   S.cons (B.str 0wx30,
                           S.append (szcode, seq)))
               end

            fun inn' s =
               let
                  val (sz, s') = inhead 0wx30 s
               
                  val (front, back) = 
                     BS.splitAt (s', sz)
                     handle Subscript => error ILLEGAL s

                  val (x, front') = inn front
               in
                  if BS.isEmpty front' then
                     (x, back)
                  else
                     error MISMATCH s
               end
         in
            (Choice [0wx30], out', inn')
         end
               

      fun tuple2 (f1, f2) =
         sequence (concat (f1, f2))

      fun tuple3 (f1, f2, f3) =
         map
         (fn (x, (y, z)) => (x, y, z)) 
         (fn (x, y, z) => (x, (y, z))) 
         (sequence (concat (f1, concat (f2, f3))))

      fun tuple4 (f1, f2, f3, f4) =
         map 
         (fn (x, (y, (z, w))) => (x, y, z, w)) 
         (fn (x, y, z, w) => (x, (y, (z, w)))) 
         (sequence (concat (f1, concat (f2, concat (f3, f4)))))

      fun tuple5 (f1, f2, f3, f4, f5) =
         map 
         (fn (x, (y, (z, (w, v)))) => (x, y, z, w, v)) 
         (fn (x, y, z, w, v) => (x, (y, (z, (w, v))))) 
         (sequence (concat (f1, concat (f2, concat (f3, concat (f4, f5))))))

      fun tuple6 (f1, f2, f3, f4, f5, f6) =
         map 
         (fn (x, (y, (z, (w, (v, u))))) => (x, y, z, w, v, u)) 
         (fn (x, y, z, w, v, u) => (x, (y, (z, (w, (v, u)))))) 
         (sequence (concat (f1, concat (f2, concat (f3, concat (f4, concat (f5, f6)))))))


      fun option (class, out, inn) =
         (case class of
             Choice ts =>
                let
                   fun out' xopt =
                      (case xopt of
                          NONE =>
                             (0, S.empty ())
                        | SOME x =>
                             out x)
    
                   fun inn' s =
                      (case BS.getc s of
                          NONE =>
                             (NONE, s)
                        | SOME (t, _) =>
                             if member t ts then
                                let
                                   val (x, s') = inn s
                                in
                                   (SOME x, s')
                                end
                             else
                                (NONE, s))
                in
                   (OptOpen ts, out', inn')
                end
           | _ =>
                raise IllegalFormat)


      fun union ((class1, out1, in1), (class2, out2, in2)) =
         (case (class1, class2) of
             (Choice ts1, Choice ts2) =>
                if overlap ts1 ts2 then
                   raise IllegalFormat
                else
                   let
                      fun out x =
                         (case x of
                             INL y => out1 y
                           | INR y => out2 y)
       
                      fun inn s =
                         (case BS.getc s of
                             NONE =>
                                error MISMATCH s
                           | SOME (t, _) =>
                                if member t ts1 then
                                   let
                                      val (x, s') = in1 s
                                   in
                                      (INL x, s')
                                   end
                                else if member t ts2 then
                                   let
                                      val (x, s') = in2 s
                                   in
                                      (INR x, s')
                                   end
                                else
                                   error MISMATCH s)
                   in
                      (Choice (ts1 @ ts2), out, inn)
                   end
           | _ =>
                raise IllegalFormat)


      fun tag (w, (_, out, inn)) =
         if w >= 0w32 then
            raise IllegalFormat
         else
            let
               val t = Word8.orb (0wxa0, w)

               fun out' x =
                  let
                     val (sz, seq) = out x
                     val (szsz, szcode) = sizeOut sz
                  in
                     (sz + szsz + 1,
                      S.cons (B.str (Word8.orb (0wxa0, t)),
                              S.append (szcode, seq)))
                  end

               fun inn' s =
                  (case BS.getc s of
                      NONE =>
                         error MISMATCH s
                    | SOME (b, s') =>
                         if b = t then
                            let
                               val (sz, s'') = sizeIn s'

                               val (front, back) =
                                  BS.splitAt (s'', sz)
                                  handle Subscript => error ILLEGAL s

                               val (x, front') = inn front
                            in
                               if BS.isEmpty front' then
                                  (x, back)
                               else
                                  error ILLEGAL s
                            end
                         else
                            error MISMATCH s)
            in
               (Choice [t], out', inn')
            end


      val integer =
         let
            fun out x =
               if IntInf.>= (x, 0) then
                  let
                     val str = C.toBytesB x
                     val hi = B.sub (str, 0)
                  in
                     if Word8.andb (hi, 0wx80) = 0w0 then
                        let
                           val sz = B.size str
                           val (szsz, szcode) = sizeOut sz
                        in
                           (sz + szsz + 1,
                            S.cons (B.str 0wx02, 
                                    S.append (szcode, S.singleton str)))
                        end
                     else
                        let
                           val sz = B.size str + 1
                           val (szsz, szcode) = sizeOut sz
                        in
                           (sz + szsz + 1,
                            S.cons (B.str 0wx02,
                                    S.append (szcode,
                                              S.cons (B.str 0wx00,
                                                      S.singleton str))))
                        end
                  end
               else
                  let
                     val str = C.toBytesB x
                     val hi = B.sub (str, 0)
                  in
                     if Word8.andb (hi, 0wx80) = 0wx80 then
                        let
                           val sz = B.size str
                           val (szsz, szcode) = sizeOut sz
                        in
                           (sz + szsz + 1,
                            S.cons (B.str 0wx02, 
                                    S.append (szcode, S.singleton str)))
                        end
                     else
                        let
                           val sz = B.size str + 1
                           val (szsz, szcode) = sizeOut sz
                        in
                           (sz + szsz + 1,
                            S.cons (B.str 0wx02,
                                    S.append (szcode,
                                              S.cons (B.str 0wxff,
                                                      S.singleton str))))
                        end
                  end

            fun inn s =
               let
                  val (sz, s') = inhead 0wx02 s
                  
                  val (front, back) =
                     BS.splitAt (s', sz)
                     handle Subscript => error ILLEGAL s

                  val hi =
                     if sz > 0 then
                        BS.sub (s', 0)
                     else
                        error ILLEGAL s

                  val sign = Word8.andb (hi, 0wx80) = 0wx80
               in
                  (C.fromSignedBytesB (sign, BS.string front), back)
               end
                      
         in
            (Choice [0wx02], out, inn)
         end


      val unsigned =
         let
            fun out x =
               if IntInf.>= (x, 0) then
                  let
                     val str = C.toBytesB x
                     val hi = B.sub (str, 0)
                  in
                     if Word8.andb (hi, 0wx80) = 0w0 then
                        let
                           val sz = B.size str
                           val (szsz, szcode) = sizeOut sz
                        in
                           (sz + szsz + 1,
                            S.cons (B.str 0wx02, 
                                    S.append (szcode, S.singleton str)))
                        end
                     else
                        let
                           val sz = B.size str + 1
                           val (szsz, szcode) = sizeOut sz
                        in
                           (sz + szsz + 1,
                            S.cons (B.str 0wx02,
                                    S.append (szcode,
                                              S.cons (B.str 0wx00,
                                                      S.singleton str))))
                        end
                  end
               else
                  raise EncodeError

            fun inn s =
               let
                  val (sz, s') = inhead 0wx02 s
                  
                  val (front, back) =
                     BS.splitAt (s', sz)
                     handle Subscript => error ILLEGAL s
               in
                  (C.fromBytesB (BS.string front), back)
               end
         in
            (Choice [0wx02], out, inn)
         end

         
      val bytestring =
         let
            fun out str =
               let
                  val sz = B.size str
                  val (szsz, szcode) = sizeOut sz
               in
                  (sz + szsz + 1,
                   S.cons (B.str 0wx04,
                           S.append (szcode, S.singleton str)))
               end

            fun inn s =
               let
                  val (sz, s') = inhead 0wx04 s

                  val (front, back) =
                     BS.splitAt (s', sz)
                     handle Subscript => error ILLEGAL s
               in
                  (BS.string front, back)
               end
         in
            (Choice [0wx04], out, inn)
         end
            

      val objectid =
         let
            fun out l =
               (case l of
                   x1 :: x2 :: rest =>
                      if
                         x1 < 0 orelse x1 > 2 orelse x2 < 0
                         orelse
                         (((x1 = 0) orelse (x1 = 1)) andalso x2 > 39)
                         orelse
                         x2 > 47
                      then
                         raise EncodeError
                      else
                         let
                            fun encloop n acc x =
                               if x = 0w0 then
                                  (n, acc)
                               else
                                  encloop
                                  (n+1)
                                  (S.cons (B.str (Word8.orb (0wx80,
                                                             ConvertWord.wordToWord8 (Word.andb (x, 0wx7f)))),
                                           acc))
                                  (Word.>> (x, 0w7))

                            fun enconto (x, (sz, seq)) =
                               let
                                  val x' = Word.fromInt x
                               in
                                  encloop
                                  (sz+1)
                                  (S.cons (B.str (ConvertWord.wordToWord8 (Word.andb (x', 0wx7f))), seq))
                                  (Word.>> (x', 0w7))
                               end

                            val (sz, seq) = foldr enconto (0, S.empty ()) rest

                            val (szsz, szcode) = sizeOut (sz+1)
                         in
                            (sz + szsz + 2,
                             S.cons (B.str (0wx06),
                                     S.append (szcode,
                                               S.cons (B.str (Word8.fromInt (x1 * 40 + x2)),
                                                       seq))))
                         end
                 | _ =>
                      raise EncodeError)

               fun inn s =
                  let
                     val (sz, s') = inhead 0wx06 s

                     val (front, back) =
                        BS.splitAt (s', sz)
                        handle Subscript => error ILLEGAL s

                     fun decone w acc l =
                        (case BS.getc l of
                            NONE =>
                               error ILLEGAL s
                          | SOME (h, t) =>
                               let
                                  val w' = 
                                     Word.orb (Word.<< (w, 0w7),
                                               ConvertWord.word8ToWord (Word8.andb (h, 0wx7f)))
                               in
                                  if Word8.andb (h, 0wx80) = 0w0 then
                                     declist (Word.toInt w' :: acc) t
                                  else
                                     decone w' acc t
                               end)

                     and declist acc l =
                        (case BS.getc l of
                            NONE =>
                               rev acc
                          | SOME _ =>
                               decone 0w0 acc l)
                                  
                  in
                     (case BS.getc front of
                         NONE =>
                            error ILLEGAL s
                       | SOME (b, front') =>
                            let
                               val h = Word8.toInt b
                            in
                               (h div 40 :: h mod 40 :: declist [] front',
                                back)
                            end)
                  end
         in
            (Choice [0wx06], out, inn)
         end


      val bitstring =
         let
            fun out (bits, extrabits) =
               let
                  val sz = B.size bits
               in
                  if extrabits < 0 orelse extrabits > 7 orelse (sz = 0 andalso extrabits > 0) then
                     raise EncodeError
                  else
                     let
                        val front = B.substring (bits, 0, sz-1)

                        (* Mask out extra bits *)
                        val last =
                           Word8.andb (B.sub (bits, sz-1),
                                       Word8.notb (Word8.- (Word8.<< (0w1, Word.fromInt extrabits),
                                                            0w1)))

                        val (szsz, szcode) = sizeOut (sz+1)
                     in
                        (sz + szsz + 2,
                         S.cons (B.str 0wx03,
                                 S.append (szcode,
                                           S.cons (B.str (Word8.fromInt extrabits),
                                                   S.cons (front,
                                                           S.singleton (B.str last))))))
                     end
               end

            fun inn s =
               let
                  val (sz, s') = inhead 0wx03 s

                  val (front, back) =
                     BS.splitAt (s', sz)
                     handle Subscript => error ILLEGAL s
               in
                  (case BS.getc front of
                      NONE =>
                         error ILLEGAL s
                    | SOME (b, front') =>
                         let
                            val extrabits = Word8.toInt b
                         in
                            if extrabits = 0 then
                               ((BS.string front', 0), back)
                            else if extrabits > 7 orelse sz - 1 = 0 then
                               error ILLEGAL s
                            else if
                               (* check that extra bits are all zero *)
                               Word8.andb (BS.sub (front, sz-1),
                                           Word8.- (Word8.<< (0w1, Word.fromInt extrabits),
                                                    0w1)) = 0w0
                            then
                               ((BS.string front', extrabits), back)
                            else
                               error ILLEGAL s
                         end)
               end
         in
            (Choice [0wx03], out, inn)
         end


      val null =
         let
            fun out () = (2, S.singleton (B.implode [0wx05, 0wx00]))

            fun inn s =
               let
                  val (sz, s') = inhead 0wx05 s
               in
                  if sz = 0 then
                     ((), s')
                  else
                     error ILLEGAL s
               end
         in
            (Choice [0wx05], out, inn)
         end


      val any =
         let
            fun out s = 
               (case BS.getc (BS.full s) of
                   NONE => raise EncodeError
                 | SOME (_, s') =>
                      let
                         val (sz, s'') = sizeIn s'
                      in
                         if BS.size s'' = sz then
                            (B.size s, S.singleton s)
                         else
                            raise EncodeError
                      end)

            fun inn s =
               (case BS.getc s of
                   NONE =>
                      error MISMATCH s
                 | SOME (_, s') =>
                      let
                         val (sz, s'') = sizeIn s'
                         
                         val (front, back) =
                            BS.splitAt (s'', sz)
                            handle Subscript => error ILLEGAL s

                         val break = BS.size s - BS.size back
                      in
                         (BS.string (BS.slice (s, 0, SOME break)), back)
                      end)
         in
            (Any, out, inn)
         end
            


      val omit =
         map (fn _ => ()) (fn () => raise EncodeError) any

      fun fixed (x, f) =
         map (fn y => if x = y then () else raise (DecodeError FIXED)) (fn () => x) f



      fun encode (_, out, _) x =
         let
            val (_, seq) = out x
         in
            B.concat (S.toList seq)
         end


      fun decode (_, _, inn) s =
         let
            val (x, s') = inn (BS.full s)
         in
            (* Apparently it's okay to have extra junk at the end of the encoding. *)
            x
         end

   end

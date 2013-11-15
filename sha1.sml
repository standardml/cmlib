
structure SHA1
   :>
   sig
      include CRYPTO_HASH
      val hashBytes : Bytestring.string -> Bytestring.string
      val hashString : string -> Bytestring.string
   end
   =
   struct

      (* Based on Wikipedia's SHA-256 pseudocode:
         http://en.wikipedia.org/wiki/SHA-1#SHA-1_pseudocode
      *)

      structure S = Stream
      structure W = Word32
      structure A = Array
      structure B = Bytestring

      val w8to32 = ConvertWord.word8ToWord32


      fun lr (w, i) =
         W.orb (W.<< (w, i),
                W.>> (w, 0w32-i))

      fun implodeWord32 (b3, b2, b1, b0) =
         W.orb (W.<< (w8to32 b3, 0w24),
                W.orb (W.<< (w8to32 b2, 0w16),
                       W.orb (W.<< (w8to32 b1, 0w8),
                              w8to32 b0)))


      fun replicateOnto i x s =
         if i <= 0 then
            s
         else
            S.eager (S.Cons (x, replicateOnto (i-1) x s))


      fun szstream i =
         let
            val sz = W.fromInt i
         in
            S.eager (S.Cons (W.>> (sz, 0w29), 
                             S.eager (S.Cons (W.<< (sz, 0w3), 
                                              S.eager S.Nil))))
         end


      fun pad i =
         let
            val j = (i div 4 + 1) mod 16
         in
            if j <= 14 then
               14-j
            else
               30-j
         end


      (* put into 32-bit chunks and add material to end *)
      fun chunk i s =
         S.lazy
         (fn () =>
             (case S.front s of
                 S.Nil =>
                    S.Cons (0wx80000000,
                            replicateOnto (pad i) 0w0 (szstream i))
               | S.Cons (b3, s3) =>
                    (case S.front s3 of
                        S.Nil =>
                           S.Cons (implodeWord32 (b3, 0wx80, 0w0, 0w0),
                                   replicateOnto (pad i) 0w0 (szstream (i+1)))
                      | S.Cons (b2, s2) =>
                           (case S.front s2 of
                               S.Nil =>
                                  S.Cons (implodeWord32 (b3, b2, 0wx80, 0w0),
                                          replicateOnto (pad i) 0w0 (szstream (i+2)))
                             | S.Cons (b1, s1) =>
                                  (case S.front s1 of
                                      S.Nil =>
                                         S.Cons (implodeWord32 (b3, b2, b1, 0wx80),
                                                 replicateOnto (pad i) 0w0 (szstream (i+3)))
                                    | S.Cons (b0, s0) =>
                                         S.Cons (implodeWord32 (b3, b2, b1, b0),
                                                 chunk (i+4) s0))))))


      type hashstate = W.word * W.word * W.word * W.word * W.word

      val hshInit : hashstate =
         (0wx67452301, 0wxEFCDAB89, 0wx98BADCFE, 0wx10325476, 0wxC3D2E1F0)
      

      fun hashBatch (hsh, cur) =
         let
            fun doextend i =
               if i >= 80 then
                  ()
               else
                  let
                     val v3 = A.sub (cur, i-3)
                     val v8 = A.sub (cur, i-8)
                     val v14 = A.sub (cur, i-14)
                     val v16 = A.sub (cur, i-16)

                     open W
                  in
                     A.update (cur, i, lr (xorb (v3, xorb (v8, xorb (v14, v16))), 0w1));
                     doextend (Int.+ (i, 1))
                  end

            fun dostep i (a, b, c, d, e) =
               if i >= 80 then
                  let
                     val (a', b', c', d', e') = hsh
                  in
                     (W.+ (a', a),
                      W.+ (b', b),
                      W.+ (c', c),
                      W.+ (d', d),
                      W.+ (e', e))
                  end
               else
                  let
                     open W
         
                     val (f, k) =
                        if Int.< (i, 20) then
                           (orb (andb (b, c), andb (notb b, d)),
                            0wx5A827999)
                        else if Int.< (i, 40) then
                           (xorb (b, xorb (c, d)),
                            0wx6ED9EBA1)
                        else if Int.< (i, 60) then
                           (orb (andb (b, c), orb (andb (b, d), andb (c, d))),
                            0wx8F1BBCDC)
                        else
                           (xorb (b, xorb (c, d)),
                            0wxCA62C1D6)

                     val temp = lr (a, 0w5) + f + e + k + A.sub (cur, i)
                  in
                     dostep (Int.+ (i, 1)) (temp, a, lr (b, 0w30), c, d)
                  end
         in
            doextend 16;
            dostep 0 hsh
         end
      
      
      fun hashStream (len, hsh, s) =
         let
            val cur : W.word A.array = A.array (80, 0w0)

            fun loadBlock i s =
               if i > 15 then
                  s
               else
                  (case S.front s of
                      S.Nil =>
                         raise (Fail "impossible")  (* |s| is a multiple of 16, so this can't happen *)
                    | S.Cons (w, s') =>
                         (
                         A.update (cur, i, w);
                         loadBlock (i+1) s'
                         ))

            fun loop (hsh, s) =
               (case S.front s of
                   S.Nil => hsh
                 | _ =>
                      let
                         val s' = loadBlock 0 s
                      in
                         loop (hashBatch (hsh, cur), s')
                      end)

            val (a, b, c, d, e) = loop (hsh, chunk len s)
         in
            B.concat
            (map ConvertWord.word32ToBytesB [a, b, c, d, e])
         end


      fun hash s = hashStream (0, hshInit, s)

      fun streamFromBytestring str = Stream.fromTable B.sub str 0
      fun streamFromString str = Stream.fromTable (fn ((), i) => Word8.fromInt (Char.ord (String.sub (str, i)))) () 0

      fun hashBytes str = hash (streamFromBytestring str)
      fun hashString str = hash (streamFromString str)


      type state = (int * hashstate * B.string)

      val initial = (0, hshInit, B.null)

      fun update ((len, hsh, accum), str) =
         let
            fun loop (st as (len, hsh, accum)) =
               if B.size accum < 64 then
                  st
               else
                  let
                     val cur : W.word A.array = A.array (80, 0w0)
   
                     fun loadBlock i s =
                        if i > 15 then
                           s
                        else
                           let
                              val h = B.substring (s, 0, 4)
                              val t = B.extract (s, 4, NONE)
                           in
                              A.update (cur, i, ConvertWord.bytesToWord32B h);
                              loadBlock (i+1) t
                           end
                           
                     val accum' = loadBlock 0 accum
   
                     val hshNext = hashBatch (hsh, cur)
                  in
                     loop (len + 64, hshNext, accum')
                  end
         in
            loop (len, hsh, B.^ (accum, str))
         end

      fun finish ((len, hsh, accum), s) =
         hashStream (len, hsh, S.@ (streamFromBytestring accum, s))

   end

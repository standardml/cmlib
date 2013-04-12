
structure RIPEMD160
   :>
   sig
      include CRYPTO_HASH
      val hashBytes : Bytestring.string -> Bytestring.string
      val hashString : string -> Bytestring.string
   end
   =
   struct

      (* Based on "RIPEMD-160: A Strengthened Version of RIPEMD" (Dobbertin, et al.):
         http://homes.esat.kuleuven.be/~bosselae/ripemd160/pdf/AB-9601/AB-9601.pdf
      *)


      structure S = Stream
      structure W = Word32
      structure A = Array
      structure V = Vector
      structure B = Bytestring

      val w8to32 = ConvertWord.word8ToWord32


      fun rl (w, i) =
         W.orb (W.<< (w, i),
                W.>> (w, 0w32-i))


      fun implodeWord32 (b0, b1, b2, b3) =
         W.orb (W.<< (w8to32 b3, 0w24),
                W.orb (W.<< (w8to32 b2, 0w16),
                       W.orb (W.<< (w8to32 b1, 0w8),
                              w8to32 b0)))


      fun replicateOnto i x s =
         if i <= 0 then
            s
         else
            S.eager (S.Cons (x, replicateOnto (i-1) x s))


      fun replicateOntoList i x l =
         if i <= 0 then
            l
         else
            x :: replicateOntoList (i-1) x l


      fun szstream i =
         let
            val sz = W.fromInt i

            val b0 = 0w0
            val b1 = W.<< (W.andb (sz, 0wx1), 0w1)
            val b2 = W.<<
         in
            S.eager (S.Cons (W.<< (sz, 0w3), 
                             S.eager (S.Cons (W.>> (sz, 0w29),
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
                    S.Cons (0wx80,
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


      local
         open W
      in

         fun f0 (x, y, z) = xorb (xorb (x, y), z)
         fun f16 (x, y, z) = orb (andb (x, y), andb (notb x, z))
         fun f32 (x, y, z) = xorb (orb (x, notb y), z)
         fun f48 (x, y, z) = orb (andb (x, z), andb (y, notb z))
         fun f64 (x, y, z) = xorb (x, orb (y, notb z))

      end

      val fv =
         V.fromList
         (List.foldr (fn (f, l) => replicateOntoList 16 f l) [] [f0, f16, f32, f48, f64])

      val kv : W.word V.vector =
         V.fromList
         (List.foldr (fn (k, l) => replicateOntoList 16 k l) []
             [0w0, 0wx5A827999, 0wx6ED9EBA1, 0wx8F1BBCDC, 0wxA953FD4E])

      val kv' : W.word V.vector =
         V.fromList
         (List.foldr (fn (k, l) => replicateOntoList 16 k l) []
             [0wx50A28BE6, 0wx5C4DD124, 0wx6D703EF3, 0wx7A6D76E9, 0w0])

      val rv =
         V.fromList
         ([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
           7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8,
           3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12,
           1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2,
           4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13])

      val rv' =
         V.fromList
         [5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12,
          6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2,
          15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13,
          8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14,
          12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11]

      val sv : Word.word V.vector =
         V.fromList
         [0w11, 0w14, 0w15, 0w12, 0w5, 0w8, 0w7, 0w9, 0w11, 0w13, 0w14, 0w15, 0w6, 0w7, 0w9, 0w8,
          0w7, 0w6, 0w8, 0w13, 0w11, 0w9, 0w7, 0w15, 0w7, 0w12, 0w15, 0w9, 0w11, 0w7, 0w13, 0w12,
          0w11, 0w13, 0w6, 0w7, 0w14, 0w9, 0w13, 0w15, 0w14, 0w8, 0w13, 0w6, 0w5, 0w12, 0w7, 0w5,
          0w11, 0w12, 0w14, 0w15, 0w14, 0w15, 0w9, 0w8, 0w9, 0w14, 0w5, 0w6, 0w8, 0w6, 0w5, 0w12,
          0w9, 0w15, 0w5, 0w11, 0w6, 0w8, 0w13, 0w12, 0w5, 0w12, 0w13, 0w14, 0w11, 0w8, 0w5, 0w6]

      val sv' : Word.word V.vector =
         V.fromList
         [0w8, 0w9, 0w9, 0w11, 0w13, 0w15, 0w15, 0w5, 0w7, 0w7, 0w8, 0w11, 0w14, 0w14, 0w12, 0w6,
          0w9, 0w13, 0w15, 0w7, 0w12, 0w8, 0w9, 0w11, 0w7, 0w7, 0w12, 0w7, 0w6, 0w15, 0w13, 0w11,
          0w9, 0w7, 0w15, 0w11, 0w8, 0w6, 0w6, 0w14, 0w12, 0w13, 0w5, 0w14, 0w13, 0w13, 0w7, 0w5,
          0w15, 0w5, 0w8, 0w11, 0w14, 0w14, 0w6, 0w14, 0w6, 0w9, 0w12, 0w9, 0w12, 0w5, 0w15, 0w8,
          0w8, 0w5, 0w12, 0w9, 0w12, 0w5, 0w14, 0w6, 0w8, 0w13, 0w6, 0w5, 0w15, 0w13, 0w11, 0w11]

      type hashstate = W.word * W.word * W.word * W.word * W.word

      val hshInit : hashstate =
         (0wx67452301, 0wxEFCDAB89, 0wx98BADCFE, 0wx10325476, 0wxC3D2E1F0)


(*
      fun hash s =
         let
            val hsh : W.word A.array = A.array (5, 0w0)
            val () = Array.copyVec {src=hshInit, dst=hsh, di=0}

            val cur : W.word A.array = A.array (16, 0w0)
      

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


            fun dostep j (a, a', b, b', c, c', d, d', e, e') =
               if j >= 80 then
                  let
                     open W
      
                     val h0 = A.sub (hsh, 0)
                     val h1 = A.sub (hsh, 1)
                     val h2 = A.sub (hsh, 2)
                     val h3 = A.sub (hsh, 3)
                     val h4 = A.sub (hsh, 4)
                  in
                     A.update (hsh, 0, h1 + c + d');
                     A.update (hsh, 1, h2 + d + e');
                     A.update (hsh, 2, h3 + e + a');
                     A.update (hsh, 3, h4 + a + b');
                     A.update (hsh, 4, h0 + b + c')
                  end
               else
                  let
                     open W
      
                     val t =
                        rl (a + V.sub (fv, j) (b, c, d) + A.sub (cur, V.sub (rv, j)) + V.sub (kv, j),
                            V.sub (sv, j))
                        + e
      
                     val t' =
                        rl (a' + V.sub (fv, Int.- (79, j)) (b', c', d') + A.sub (cur, V.sub (rv', j)) + V.sub (kv', j),
                            V.sub (sv', j))
                        + e'
                  in
                     dostep (Int.+ (j, 1))
                     (e, e', t, t', b, b', rl (c, 0w10), rl (c', 0w10), d, d')
                  end


            fun doHash s =
               (case S.front s of
                   S.Nil =>
                      ()
                 | _ =>
                      let
                         val s' = loadBlock 0 s
      
                         val () =
                            dostep 0
                               (A.sub (hsh, 0),
                                A.sub (hsh, 0),
                                A.sub (hsh, 1),
                                A.sub (hsh, 1),
                                A.sub (hsh, 2),
                                A.sub (hsh, 2),
                                A.sub (hsh, 3),
                                A.sub (hsh, 3),
                                A.sub (hsh, 4),
                                A.sub (hsh, 4))
      
                      in
                         doHash s'
                      end)
      
      
            val () = doHash (chunk 0 s)
         in
            Bytestring.concat
            (Array.foldr (fn (w, l) => ConvertWord.word32ToBytesL w :: l) [] hsh)
         end
*)

      fun hashBatch ((h0, h1, h2, h3, h4), cur) =
         let
            fun dostep j (a, a', b, b', c, c', d, d', e, e') =
               if j >= 80 then
                  let
                     open W
                  in
                     (h1 + c + d',
                      h2 + d + e',
                      h3 + e + a',
                      h4 + a + b',
                      h0 + b + c')
                  end
               else
                  let
                     open W
      
                     val t =
                        rl (a + V.sub (fv, j) (b, c, d) + A.sub (cur, V.sub (rv, j)) + V.sub (kv, j),
                            V.sub (sv, j))
                        + e
      
                     val t' =
                        rl (a' + V.sub (fv, Int.- (79, j)) (b', c', d') + A.sub (cur, V.sub (rv', j)) + V.sub (kv', j),
                            V.sub (sv', j))
                        + e'
                  in
                     dostep (Int.+ (j, 1))
                     (e, e', t, t', b, b', rl (c, 0w10), rl (c', 0w10), d, d')
                  end
         in
            dostep 0 (h0, h0, h1, h1, h2, h2, h3, h3, h4, h4)
         end


      fun hashStream (len, hsh, s) =
         let
            val cur : W.word A.array = A.array (16, 0w0)
            
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

            val (h0, h1, h2, h3, h4) = loop (hsh, chunk len s)
         in
            B.concat
            (map ConvertWord.word32ToBytesL [h0, h1, h2, h3, h4])
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
                     val cur : W.word A.array = A.array (64, 0w0)
   
                     fun loadBlock i s =
                        if i > 15 then
                           s
                        else
                           let
                              val h = B.substring (s, 0, 4)
                              val t = B.extract (s, 4, NONE)
                           in
                              A.update (cur, i, ConvertWord.bytesToWord32L h);
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

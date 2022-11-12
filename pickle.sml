
structure Pickle :> PICKLE =
   struct

      structure J = JenkinsHash
      structure MJ = MJHash
      structure A = Word8Array
      structure AS = Word8ArraySlice

      type byte = Word8.word
      type consumer = byte -> unit
      type producer = unit -> byte
      
      exception Error

      (* A pickler takes two consumers.  The first is the consumer for ordinary data,
         the second is the consumer for administrative data.  The idea is that repeated
         pickling of the same data structure will always result in the same ordinary data,
         but the administrative data might vary.

         Usually they are the same, but sometimes the administrative consumer does nothing.
         This is useful in sharing, where sometimes we need to suppress the administrative
         data.  When we pickle something into a string to store in the sharing data, we want
         to leave the administrative data out, since it might vary.
      *)

      datatype 'a pu =
         PU of { pick : consumer -> consumer -> 'a -> unit,
                 unpick : producer -> 'a,
                 cleanup : unit -> unit }

      val noop = fn _ => ()

      val unit =
         PU { pick    = (fn _ => fn _ => fn _ => ()),
              unpick  = (fn _ => ()),
              cleanup = noop }

      fun pBool (outf : consumer) _ b =
         if b then
            outf 0w1
         else
            outf 0w0

      val bool =
         PU { pick = pBool,

              unpick = 
                 (fn inf => 
                     let
                        val w = inf ()
                     in
                        if w = 0w0 then
                           false
                        else if w = 0w1 then
                           true
                        else
                           raise Error
                     end),

              cleanup = noop }

      (* first byte                  subsequent bytes
               7      6         5-0        7    6-0 
         +------+------+-----------+ +------+------+ 
         | sign | more | l.s. bits | | more | bits | ...
         +------+------+-----------+ +------+------+

         more=1 means more bytes to come
         first byte 128 is not used (would be -0)
      *)

      val signbit = Word8.fromInt 0x80
      val morebit1 = Word8.fromInt 0x40
      val morebitn = Word8.fromInt 0x80

      val mask1 : IntInf.int = 0x40 - 1
      val maskn : IntInf.int = 0x80 - 1

      fun pIntInfLoop outf n =
         if n <= maskn then
            outf (ConvertWord.intInfToWord8 n)
         else
            (
            outf (Word8.orb (ConvertWord.intInfToWord8 (IntInf.andb (n, maskn)), morebitn));
            pIntInfLoop outf (IntInf.~>> (n, 0w7))
            )

      fun pIntInf outf n =
         let
            val sign = if n < 0 then signbit else 0w0
            val n = IntInf.abs n
         in
            if n <= mask1 then
               outf (Word8.orb (ConvertWord.intInfToWord8 n, sign))
            else
               (
               outf (Word8.orb (ConvertWord.intInfToWord8 (IntInf.andb (n, mask1)), Word8.orb (sign, morebit1)));
               pIntInfLoop outf (IntInf.~>> (n, 0w6))
               )
         end

      fun pIntInf' outf _ n = pIntInf outf n

      fun uIntInfLoop inf acc =
         let
            val b = inf ()
         in
            if Word8.andb (b, morebitn) = 0w0 then
               ConvertWord.word8ToIntInf b :: acc
            else
               uIntInfLoop inf (ConvertWord.word8ToIntInf (Word8.andb (b, Word8.notb morebitn)) :: acc)
         end

      fun uIntInf inf =
         let
            val b = inf ()

            val b' = Word8.andb (b, Word8.notb signbit)

            val n =
               if Word8.andb (b', morebit1) = 0w0 then
                  ConvertWord.word8ToIntInf b'
               else
                  IntInf.<<
                     (foldl
                         (fn (c, n) => IntInf.<< (n, 0w7) + c)
                         0
                         (uIntInfLoop inf []),
                      0w6)
                  + ConvertWord.word8ToIntInf (Word8.andb (b', Word8.notb morebit1))
         in
            if Word8.andb (b, signbit) = 0w0 then
               n
            else
               ~ n
         end

      val intInf =
         PU { pick    = pIntInf',
              unpick  = uIntInf,
              cleanup = noop }

      fun pInt outf n = pIntInf outf (IntInf.fromInt n)

      fun pInt' outf _ n = pIntInf outf (IntInf.fromInt n)

      fun uInt inf = 
         IntInf.toInt (uIntInf inf)
         handle Overflow => raise Error

      val int =
         PU { pick    = pInt',
              unpick  = uInt,
              cleanup = noop }

      fun pChar outf ch =
         let val n = Char.ord ch
            in
               if n > 255 then
                  raise Error
               else
                  outf (Word8.fromInt n)
            end

      fun pChar' outf _ ch = pChar outf ch

      fun uChar inf = Char.chr (Word8.toInt (inf ()))
         
      val char =
         PU { pick    = pChar',
              unpick  = uChar,
              cleanup = noop }

      fun pStringLoop outf str i len =
         if i >= len then
            ()
         else
            (
            pChar outf (String.sub (str, i));
            pStringLoop outf str (i+1) len
            )

      fun pString outf _ str =
         let
            val len = String.size str
         in
            pInt outf len;
            pStringLoop outf str 0 len
         end

      fun uStringLoop inf n acc =
         if n = 0 then
            acc
         else
            uStringLoop inf (n-1) (uChar inf :: acc)
            
      fun uString inf =
         let
            val len = uInt inf
         in
            String.implode (rev (uStringLoop inf len []))
         end

      val string =
         PU { pick    = pString,
              unpick  = uString,
              cleanup = noop }

      fun pWord8 outf w = outf w

      fun pWord8' outf _ w = pWord8 outf w

      fun uWord8 inf = inf ()

      val word8 =
         PU { pick    = pWord8',
              unpick  = uWord8,
              cleanup = noop }

      val lowbyte = Word32.fromInt 255

      fun pWord32 outf w =
         let
            val b0 = Word32.andb (w, lowbyte)
            val w1 = Word32.>> (w, 0w8)
            val b1 = Word32.andb (w1, lowbyte)
            val w2 = Word32.>> (w1, 0w8)
            val b2 = Word32.andb (w2, lowbyte)
            val b3 = Word32.>> (w2, 0w8)
         in
            outf (ConvertWord.word32ToWord8 b3);
            outf (ConvertWord.word32ToWord8 b2);
            outf (ConvertWord.word32ToWord8 b1);
            outf (ConvertWord.word32ToWord8 b0)
         end

      fun pWord32' outf _ w = pWord32 outf w

      fun uWord32 inf =
         let
            val b3 = inf ()
            val b2 = inf ()
            val b1 = inf ()
            val b0 = inf ()
         in
            Word32.orb (Word32.<< (Word32.orb (Word32.<< (Word32.orb (Word32.<< (ConvertWord.word8ToWord32 b3, 
                                                                                 0w8),
                                                                      ConvertWord.word8ToWord32 b2),
                                                          0w8),
                                               ConvertWord.word8ToWord32 b1),
                                   0w8),
                        ConvertWord.word8ToWord32 b0)
         end

      val word32 =
         PU { pick    = pWord32',
              unpick  = uWord32,
              cleanup = noop }

      fun pair 
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         =
         PU { pick =
                 (fn outf => fn admf => fn (x, y) =>
                     (
                     p1 outf admf x;
                     p2 outf admf y
                     )),
                 
              unpick =
                 (fn inf =>
                     let
                        val x = u1 inf
                        val y = u2 inf
                     in
                        (x, y)
                     end),

              cleanup =
                 (fn () => (c1 (); c2 ())) }

      val tuple2 = pair

      fun tuple3
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         (PU {pick=p3, unpick=u3, cleanup=c3})
         =
         PU { pick =
                 (fn outf => fn admf => fn (x, y, z) =>
                     (
                     p1 outf admf x;
                     p2 outf admf y;
                     p3 outf admf z
                     )),
                 
              unpick =
                 (fn inf =>
                     let
                        val x = u1 inf
                        val y = u2 inf
                        val z = u3 inf
                     in
                        (x, y, z)
                     end),

              cleanup =
                 (fn () => (c1 (); c2 (); c3 ())) }

      fun tuple4
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         (PU {pick=p3, unpick=u3, cleanup=c3})
         (PU {pick=p4, unpick=u4, cleanup=c4})
         =
         PU { pick =
                 (fn outf => fn admf => fn (x, y, z, w) =>
                     (
                     p1 outf admf x;
                     p2 outf admf y;
                     p3 outf admf z;
                     p4 outf admf w
                     )),
                 
              unpick =
                 (fn inf =>
                     let
                        val x = u1 inf
                        val y = u2 inf
                        val z = u3 inf
                        val w = u4 inf
                     in
                        (x, y, z, w)
                     end),

              cleanup =
                 (fn () => (c1 (); c2 (); c3 (); c4 ())) }

      fun tuple5
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         (PU {pick=p3, unpick=u3, cleanup=c3})
         (PU {pick=p4, unpick=u4, cleanup=c4})
         (PU {pick=p5, unpick=u5, cleanup=c5})
         =
         PU { pick =
                 (fn outf => fn admf => fn (x, y, z, w, v) =>
                     (
                     p1 outf admf x;
                     p2 outf admf y;
                     p3 outf admf z;
                     p4 outf admf w;
                     p5 outf admf v
                     )),
                 
              unpick =
                 (fn inf =>
                     let
                        val x = u1 inf
                        val y = u2 inf
                        val z = u3 inf
                        val w = u4 inf
                        val v = u5 inf
                     in
                        (x, y, z, w, v)
                     end),

              cleanup =
                 (fn () => (c1 (); c2 (); c3 (); c4 (); c5 ())) }

      fun tuple6
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         (PU {pick=p3, unpick=u3, cleanup=c3})
         (PU {pick=p4, unpick=u4, cleanup=c4})
         (PU {pick=p5, unpick=u5, cleanup=c5})
         (PU {pick=p6, unpick=u6, cleanup=c6})
         =
         PU { pick =
                 (fn outf => fn admf => fn (x, y, z, w, v, u) =>
                     (
                     p1 outf admf x;
                     p2 outf admf y;
                     p3 outf admf z;
                     p4 outf admf w;
                     p5 outf admf v;
                     p6 outf admf u
                     )),
                 
              unpick =
                 (fn inf =>
                     let
                        val x = u1 inf
                        val y = u2 inf
                        val z = u3 inf
                        val w = u4 inf
                        val v = u5 inf
                        val u = u6 inf
                     in
                        (x, y, z, w, v, u)
                     end),

              cleanup =
                 (fn () => (c1 (); c2 (); c3 (); c4 (); c5 (); c6 ())) }

      fun tuple7
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         (PU {pick=p3, unpick=u3, cleanup=c3})
         (PU {pick=p4, unpick=u4, cleanup=c4})
         (PU {pick=p5, unpick=u5, cleanup=c5})
         (PU {pick=p6, unpick=u6, cleanup=c6})
         (PU {pick=p7, unpick=u7, cleanup=c7})
         =
         PU { pick =
                 (fn outf => fn admf => fn (x, y, z, w, v, u, t) =>
                     (
                     p1 outf admf x;
                     p2 outf admf y;
                     p3 outf admf z;
                     p4 outf admf w;
                     p5 outf admf v;
                     p6 outf admf u;
                     p7 outf admf t
                     )),
                 
              unpick =
                 (fn inf =>
                     let
                        val x = u1 inf
                        val y = u2 inf
                        val z = u3 inf
                        val w = u4 inf
                        val v = u5 inf
                        val u = u6 inf
                        val t = u7 inf
                     in
                        (x, y, z, w, v, u, t)
                     end),

              cleanup =
                 (fn () => (c1 (); c2 (); c3 (); c4 (); c5 (); c6 (); c7 ())) }

      fun tuple8
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         (PU {pick=p3, unpick=u3, cleanup=c3})
         (PU {pick=p4, unpick=u4, cleanup=c4})
         (PU {pick=p5, unpick=u5, cleanup=c5})
         (PU {pick=p6, unpick=u6, cleanup=c6})
         (PU {pick=p7, unpick=u7, cleanup=c7})
         (PU {pick=p8, unpick=u8, cleanup=c8})
         =
         PU { pick =
                 (fn outf => fn admf => fn (x, y, z, w, v, u, t, s) =>
                     (
                     p1 outf admf x;
                     p2 outf admf y;
                     p3 outf admf z;
                     p4 outf admf w;
                     p5 outf admf v;
                     p6 outf admf u;
                     p7 outf admf t;
                     p8 outf admf s
                     )),
                 
              unpick =
                 (fn inf =>
                     let
                        val x = u1 inf
                        val y = u2 inf
                        val z = u3 inf
                        val w = u4 inf
                        val v = u5 inf
                        val u = u6 inf
                        val t = u7 inf
                        val s = u8 inf
                     in
                        (x, y, z, w, v, u, t, s)
                     end),

              cleanup =
                 (fn () => (c1 (); c2 (); c3 (); c4 (); c5 (); c6 (); c7 (); c8 ())) }

      fun tuple9
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         (PU {pick=p3, unpick=u3, cleanup=c3})
         (PU {pick=p4, unpick=u4, cleanup=c4})
         (PU {pick=p5, unpick=u5, cleanup=c5})
         (PU {pick=p6, unpick=u6, cleanup=c6})
         (PU {pick=p7, unpick=u7, cleanup=c7})
         (PU {pick=p8, unpick=u8, cleanup=c8})
         (PU {pick=p9, unpick=u9, cleanup=c9})
         =
         PU { pick =
                 (fn outf => fn admf => fn (x, y, z, w, v, u, t, s, r) =>
                     (
                     p1 outf admf x;
                     p2 outf admf y;
                     p3 outf admf z;
                     p4 outf admf w;
                     p5 outf admf v;
                     p6 outf admf u;
                     p7 outf admf t;
                     p8 outf admf s;
                     p9 outf admf r
                     )),
                 
              unpick =
                 (fn inf =>
                     let
                        val x = u1 inf
                        val y = u2 inf
                        val z = u3 inf
                        val w = u4 inf
                        val v = u5 inf
                        val u = u6 inf
                        val t = u7 inf
                        val s = u8 inf
                        val r = u9 inf
                     in
                        (x, y, z, w, v, u, t, s, r)
                     end),

              cleanup =
                 (fn () => (c1 (); c2 (); c3 (); c4 (); c5 (); c6 (); c7 (); c8 (); c9 ())) }

      fun tuple10
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         (PU {pick=p3, unpick=u3, cleanup=c3})
         (PU {pick=p4, unpick=u4, cleanup=c4})
         (PU {pick=p5, unpick=u5, cleanup=c5})
         (PU {pick=p6, unpick=u6, cleanup=c6})
         (PU {pick=p7, unpick=u7, cleanup=c7})
         (PU {pick=p8, unpick=u8, cleanup=c8})
         (PU {pick=p9, unpick=u9, cleanup=c9})
         (PU {pick=p10, unpick=u10, cleanup=c10})
         =
         PU { pick =
                 (fn outf => fn admf => fn (x, y, z, w, v, u, t, s, r, q) =>
                     (
                     p1 outf admf x;
                     p2 outf admf y;
                     p3 outf admf z;
                     p4 outf admf w;
                     p5 outf admf v;
                     p6 outf admf u;
                     p7 outf admf t;
                     p8 outf admf s;
                     p9 outf admf r;
                     p10 outf admf q
                     )),
                 
              unpick =
                 (fn inf =>
                     let
                        val x = u1 inf
                        val y = u2 inf
                        val z = u3 inf
                        val w = u4 inf
                        val v = u5 inf
                        val u = u6 inf
                        val t = u7 inf
                        val s = u8 inf
                        val r = u9 inf
                        val q = u10 inf
                     in
                        (x, y, z, w, v, u, t, s, r, q)
                     end),

              cleanup =
                 (fn () => (c1 (); c2 (); c3 (); c4 (); c5 (); c6 (); c7 (); c8 (); c9 (); c10 ())) }


      fun pList (p : consumer -> consumer -> 'a -> unit) outf admf l =
         (
         List.app
            (fn x =>
                (
                outf 0w1;
                p outf admf x
                ))
            l;
            
         outf 0w0
         )

      fun uListLoop (p : producer -> 'a) inf acc =
         (case inf () of
             0w0 => rev acc

           | 0w1 =>
                uListLoop p inf (p inf :: acc)

           | _ =>
                raise Error)

      fun uList p inf = uListLoop p inf []

      fun list (PU { pick, unpick, cleanup }) =
         PU { pick    = pList pick,
              unpick  = uList unpick,
              cleanup = cleanup }

      fun option (PU { pick, unpick, cleanup }) =
         PU { pick =
                 (fn outf => fn admf => (fn NONE => outf 0w0
                                          | SOME x =>
                                               (
                                               outf 0w1;
                                               pick outf admf x
                                               ))),

              unpick =
                 (fn inf => (case inf () of
                                0w0 => NONE

                              | 0w1 =>
                                   SOME (unpick inf)

                              | _ =>
                                   raise Error)),

              cleanup = cleanup }

      fun sum
         (PU {pick=p1, unpick=u1, cleanup=c1}) 
         (PU {pick=p2, unpick=u2, cleanup=c2})
         =
         PU { pick =
                 (fn outf => fn admf => (fn Sum.INL x =>
                                               (
                                               outf 0w0;
                                               p1 outf admf x
                                               )
                                          | Sum.INR x =>
                                               (
                                               outf 0w1;
                                               p2 outf admf x
                                               ))),

              unpick =
                 (fn inf => (case inf () of
                                0w0 =>
                                   Sum.INL (u1 inf)

                              | 0w1 =>
                                   Sum.INR (u2 inf)

                              | _ =>
                                   raise Error)),

              cleanup = (fn () => (c1 (); c2 ())) }

      fun wrap g f (PU { pick, unpick, cleanup }) =
         PU { pick =
                 (fn outf => fn admf => fn x => pick outf admf (g x)),

              unpick =
                 (fn inf => f (unpick inf)),

              cleanup = cleanup }

      fun fix puf =
         let
            val p = ref (fn _ => fn _ => fn _ => ())
            val u = ref (fn _ => raise Error)

            val pu =
               PU { pick =
                       (fn outf => !p outf),
   
                    unpick =
                       (fn inf => !u inf),
   
                    cleanup = noop }

            val pu' as PU { pick, unpick, ...} = puf pu
         in
            p := pick;
            u := unpick;
            pu'
         end

      fun const x =
         PU { pick    = (fn _ => fn _ => fn _ => ()),
              unpick  = (fn _ => x),
              cleanup = noop }

      fun susp f =
         PU { pick    = (fn _ => fn _ => fn _ => ()),
              unpick  = (fn _ => f ()),
              cleanup = noop }

      fun altgen f (PU { pick=pickb, unpick=unpickb, cleanup=cleanupb }) pufn appa =
         PU {
            pick =
               (fn outf => fn admf => fn a =>
                   let
                      val b = f a

                      val PU { pick=picka, ... } = pufn b
                   in
                      pickb outf admf b;
                      picka outf admf a
                   end),

            unpick =
               (fn inf =>
                   let
                      val b = unpickb inf
                      
                      val PU { unpick=unpicka, ... } = pufn b
                   in
                      unpicka inf
                   end),

            cleanup =
               (fn () =>
                   (
                   cleanupb ();
                   appa (fn PU { cleanup, ... } => cleanup ())
                   ))
            }

      fun alt f arms =
         let
            val arms = Vector.fromList arms
         in
            altgen f int 
               (fn i => Vector.sub (arms, i) handle Subscript => raise Error)
               (fn g => Vector.app g arms)
         end


      fun pListish app (p : consumer -> consumer -> 'a -> unit) outf admf l =
         (
         app 
            (fn x =>
                (
                outf 0w1;
                p outf admf x
                ))
            l;

         outf 0w0
         )

      fun uListishLoop c (p : producer -> 'a) inf acc =
         (case inf () of
             0w0 => acc

           | 0w1 =>
                uListishLoop c p inf (c (p inf, acc))

           | _ =>
                raise Error)

      fun uListish n c p inf = uListishLoop c p inf (n ())

      fun listish n c app (PU { pick, unpick, cleanup }) =
         PU { pick    = pListish app pick,
              unpick  = uListish n c unpick,
              cleanup = cleanup }

      structure B
         :>
         sig

            type working
            type complete

            val new : unit -> working

            val write : working -> consumer
            val complete : working -> complete

            val dump : consumer -> complete -> unit
            val hash : complete -> Word.word
            val eq : (complete * complete) -> bool
            val size : complete -> int

         end
         =
         struct

            val framesize = 32

            type working = int ref * int ref * A.array ref * AS.slice list ref
            type complete = int * AS.slice list
      
            fun new () : working = (ref 0, ref 0, ref (A.array (framesize, 0w0)), ref nil)

            fun write (size, pos, front, rest) b =
               (
               A.update (!front, !pos, b);
               pos := !pos + 1;
               size := !size + 1
               )
               handle Subscript =>
                  let
                     val a = A.array (framesize, 0w0)
                  in
                     A.update (a, 0, b);
                     pos := 1;
                     rest := AS.full (!front) :: !rest;
                     front := a;
                     size := !size + 1
                  end

            fun complete (size, pos, front, rest) =
               (!size, rev (AS.slice (!front, 0, SOME (!pos)) :: !rest))

            val empty = Word8ArraySlice.full (Word8Array.array (0, 0w0))

            fun front i arr buf =
               SOME (AS.sub (arr, i), i+1, arr, buf)
               handle Subscript =>
                  (case buf of
                      nil => NONE
                    | h :: t => front 0 h t)

            fun dumpLoop outf i arr buf =
               (case front i arr buf of
                   NONE => ()

                 | SOME (b, i', arr', buf') =>
                      (
                      outf b;
                      dumpLoop outf i' arr' buf'
                      ))
            
            fun dump outf (_, buf) = dumpLoop outf 0 empty buf

            fun hashLoop i arr buf acc =
               (case front i arr buf of
                   NONE => acc

                 | SOME (b, i', arr', buf') =>
                      hashLoop i' arr' buf'
                      (J.hashInc acc (ConvertWord.word8ToWord b)))

            fun hash (_, buf) = hashLoop 0 empty buf 0w0
      
            fun eqLoop i arr1 buf1 j arr2 buf2 =
               (case (front i arr1 buf1, front j arr2 buf2) of
                   (NONE, NONE) => true
      
                 | (SOME (b, i', arr1', buf1'), SOME (c, j', arr2', buf2')) =>
                      b = c
                      andalso
                      eqLoop i' arr1' buf1' j' arr2' buf2'

                 | _ => false)
      
            fun eq ((sz1:int, buf1), (sz2, buf2)) =
               sz1 = sz2
               andalso
               eqLoop 0 empty buf1 0 empty buf2

            fun size (sz, _) = sz

         end
         
      type key = word * B.complete

      structure Table =
         HashTable (structure Key =
                       struct
                          type t = key

                          fun hash (h, _) = h

                          fun eq ((_, buf), (_, buf')) = B.eq (buf, buf')
                       end)

      fun share (PU { pick, unpick, cleanup } : 'a pu) =
         let
            val table : int Table.table = Table.table 31
            val next = ref 0

            val arr : 'a option ArrayInf.array = ArrayInf.array NONE

            fun pShare outf admf x =
               let
                  val buf = B.new ()
                  val bufAll = B.new ()

                  fun outf' b =
                     (
                     B.write buf b;
                     B.write bufAll b
                     )

                  fun admf' b =
                     B.write bufAll b

                  val () = pick outf' admf' x

                  val comp = B.complete buf

                  val (i, present) =
                     Table.lookupOrInsert' table (B.hash comp, comp)
                     (fn () =>
                         let
                            val i = !next
                         in
                            next := i + 1;
                            i
                         end)
               in
                  pInt outf i;

                  if present then
                     ()
                  else
                     B.dump admf (B.complete bufAll)
               end

            fun uShare inp =
               let
                  val i = uInt inp
               in
                  (case ArrayInf.sub (arr, i) of
                      NONE =>
                         let
                            val x = unpick inp
                         in
                            ArrayInf.update (arr, i, SOME x);
                            x
                         end

                    | SOME x => x)
               end
            
            fun cleanup' () =
               (
               Table.reset table 31;
               next := 0;
               ArrayInf.erase arr;
               cleanup ()
               )
         in
            PU { pick    = pShare,
                 unpick  = uShare,
                 cleanup = cleanup' }
         end


      (* This won't work if a share is within a protect within a share.
         The buffering structure doesn't distinguish between the two
         interleaved sorts of data.  That only matters if the two consumers
         are different (i.e., when within a share), and when something
         different is written to the two consumers (i.e., when the sub-pickler
         contains a share.

         We could extend the buffer to handle this, but I don't currently
         have a use for that.
      *)

      fun protect (PU { pick, unpick, cleanup }) =
         let
            fun pProtect outf _ x =
               let
                  val buf = B.new ()

                  val outf' = B.write buf
                  
                  val () = pick outf' outf' x

                  val comp = B.complete buf
               in
                  pInt outf (B.size comp);
                  B.dump outf comp
               end

            fun uProtect inp =
               let
                  val _ = uInt inp
               in
                  unpick inp
               end
         in
            PU { pick = pProtect, unpick = uProtect, cleanup = noop }
         end

      fun pSkip outf _ () = pInt outf 0

      fun skipLoop inf n =
         if n = 0 then
            ()
         else
            (
            inf ();
            skipLoop inf (n-1)
            )

      fun uSkip inf =
         let
            val n = uInt inf
         in
            skipLoop inf n;
            ()
         end

      val skipProtect =
         PU { pick = pSkip,
              unpick = uSkip,
              cleanup = noop }


      fun lift f =
         PU { pick =
                 (fn x => fn y => fn z =>
                     let
                        val PU { pick, ... } = f ()
                     in
                        pick x y z
                     end),

              unpick = 
                 (fn x =>
                     let
                        val PU { unpick, ... } = f ()
                     in
                        unpick x
                     end),

              cleanup =
                 (fn () =>
                     let
                        val PU { cleanup, ... } = f ()
                     in
                        cleanup ()
                     end) }


      fun pickle outf (PU { pick, ... }) x = pick outf outf x
      fun unpickle inf (PU { unpick, ... }) = unpick inf
      fun reset (PU { cleanup, ... }) = cleanup ()

   end

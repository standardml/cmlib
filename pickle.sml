
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
            outf 0w0
         else
            outf 0w1

      val bool =
         PU { pick = pBool,

              unpick = 
                 (fn inf => (case inf () of
                                0w0 => false
                              | 0w1 => true
                              | _ => raise Error)),

              cleanup = noop }

      (* first byte                  subsequent bytes
               7      6         5-0        7    6-0 
         +------+------+-----------+ +------+------+ 
         | sign | more | l.s. bits | | more | bits | ...
         +------+------+-----------+ +------+------+

         more=1 means more bytes to come
         first byte 128 is not used (would be -0)
      *)

      val signbit : Word8.word = 0wx80
      val morebit1 : Word8.word = 0wx40
      val morebitn : Word8.word = 0wx80

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

      fun triple
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

      fun quad
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

      fun alt f arms =
         let
            val arms = Vector.fromList arms
         in
            PU { pick =
                    (fn outf => fn admf => fn x =>
                        let
                           val n = f x

                           val PU { pick=p, ... } =
                              Vector.sub (arms, n)
                              handle Subscript => raise Error
                        in
                           pInt outf n;
                           p outf admf x
                        end),

                 unpick =
                    (fn inf =>
                        let
                           val n = uInt inf

                           val PU { unpick = u, ... } =
                              Vector.sub (arms, n)
                              handle Subscript => raise Error
                        in
                           u inf
                        end),

                 cleanup =
                    (fn () => Vector.app (fn (PU { cleanup, ... }) => cleanup ()) arms) }
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

         end
         =
         struct

            val framesize = 32

            type working = int ref * A.array ref * AS.slice list ref
            type complete = AS.slice list
      
            fun new () : working = (ref 0, ref (A.array (framesize, 0w0)), ref nil)

            fun write (pos, front, rest) b =
               (
               A.update (!front, !pos, b);
               pos := !pos + 1
               )
               handle Subscript =>
                  let
                     val a = A.array (framesize, 0w0)
                  in
                     A.update (a, 0, b);
                     pos := 1;
                     rest := AS.full (!front) :: !rest;
                     front := a
                  end

            fun complete (pos, front, rest) =
               rev (AS.slice (!front, 0, SOME (!pos)) :: !rest)

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
            
            fun dump outf buf = dumpLoop outf 0 empty buf

            fun hashLoop i arr buf acc =
               (case front i arr buf of
                   NONE => acc

                 | SOME (b, i', arr', buf') =>
                      hashLoop i' arr' buf'
                      (J.hashInc acc (ConvertWord.word8ToWord b)))

            fun hash buf = hashLoop 0 empty buf 0w0
      
            fun eqLoop i arr1 buf1 j arr2 buf2 =
               (case (front i arr1 buf1, front j arr2 buf2) of
                   (NONE, NONE) => true
      
                 | (SOME (b, i', arr1', buf1'), SOME (c, j', arr2', buf2')) =>
                      b = c
                      andalso
                      eqLoop i' arr1' buf1' j' arr2' buf2'

                 | _ => false)
      
            fun eq (buf1, buf2) =
               eqLoop 0 empty buf1 0 empty buf2

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

      fun pickle (PU { pick, ... }) outf x = pick outf outf x
      fun unpickle (PU { unpick, ... }) inf = unpick inf
      fun reset (PU { cleanup, ... }) = cleanup ()

   end

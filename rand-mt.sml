
(* Mersenne Twister algorithm *)

structure MTRand32 :> RAND32 where type seed = Word32.word =
   struct

      structure A = Array
      structure W = Word32
      type word32 = Word32.word

      val degree = 624
      val middle = 397
      val twist : word32 = 0wx9908b0df
      val tempermask1 : word32 = 0wx9d2c5680
      val tempermask2 : word32 = 0wxefc60000

      val indexr = ref 0
      val mt : word32 A.array = Array.array (degree, 0w0)

      type seed = word32

      fun reseed seed =
         let
            fun initLoop i =
               if i >= degree then
                  ()
               else
                  (
                  A.update (mt, i,
                            0w1812433253 
                            * (W.xorb (A.sub (mt, i-1), W.>> (A.sub (mt, i-1), 0w30)))
                            + Word32.fromInt i);
                  initLoop (i+1)
                  )
         in
            A.update (mt, 0, seed);
            initLoop 1
         end

      val () = reseed (Word32.fromLargeInt (Time.toMilliseconds (Time.now ())))

      fun generateRaw i =
         if i >= degree then
            ()
         else
            let
               val y =
                  W.orb (W.andb (A.sub (mt, i), 0wx80000000),
                         W.andb (A.sub (mt, (i+1) mod degree), 0wx7fffffff))

               val z =
                  W.xorb (A.sub (mt, (i + middle) mod degree), W.>> (y, 0w1))
            in
               if W.andb (y, 0w1) = 0w0 then
                  A.update (mt, i, z)
               else
                  A.update (mt, i, W.xorb (z, twist));

               generateRaw (i+1)
            end
         
      fun randWord32 () =
         let
            val i = !indexr
            val () = indexr := (i + 1) mod degree

            val () =
               if i = 0 then
                  generateRaw 0
               else
                  ()
            
            val y = A.sub (mt, i)
            val y = W.xorb (y, W.>> (y, 0w11))
            val y = W.xorb (y, W.andb (W.<< (y, 0w7), tempermask1))
            val y = W.xorb (y, W.andb (W.<< (y, 0w15), tempermask2))
            val y = W.xorb (y, W.>> (y, 0w18))
         in
            y
         end

   end

structure MTRand = RandFromRand32 (structure Rand32 = MTRand32)

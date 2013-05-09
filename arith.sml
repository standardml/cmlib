
functor ArithFun (structure Rand : RAND)
   :> ARITH =
   struct

      open IntInf


      fun sqrt n =
         let
            (* Let x_(i+1) = floor ( (xi^2 + n) / 2xi )
               Denote x_(i+1) by xi'
               Let ai = (xi^2 + n) / 2xi - xi'
               Note that 0 <= ai < 1.

               Lemma: If xi > 0 and xi > xi' then xi^2 > n.
               
               xi > xi' + ai  (since xi and xi' are integers and ai < 1)
                  = (x^2 + n) / 2xi
               so 2xi^2 > x^2 + n
               so xi^2 > n
               QED

               Corollary: If n > xi > 0 then xi' > 0.

               xi' =  floor ( (xi^2 + n) / 2xi )
                   >= floor ( 2n / 2xi )   (by the lemma)
                   =  floor ( n / xi )
                   >= 1
               QED

               Thus, as long as xi falls, xi > 0.  (As soon as xi rises, we're done.)

               Theorem: If xi > xi' <= xi'' and xi > 0 then xi'^2 <= n < (xi' + 1)^2

               floor ( (xi'^2 + n) / 2xi' ) = xi'' >= xi'
               so (xi'^2 + n) / 2xi' >= xi'
               so xi'^2 + n >= 2xi'^2
               so n >= xi'^2.

               It remains to show (xi' + 1)^2 > n.
               xi' = (xi^2 + n) / 2xi - ai
                   = xi - (xi^2 - n)/2xi - ai
               so (xi' + 1)^2 = (xi - (xi^2 - n)/2xi - ai + 1)^2
                              > (xi - (xi^2 - n)/2xi)^2  (since 1 - ai > 0)
                              = xi^2 - (xi^2 - n) + ((xi^2 - n)/2xi)^2
                              = n + ((xi^2 - n)/2xi)^2
                              > n   (since xi^2 - n > 0)

               QED
            *)

            fun loop est =
               let
                  val est' = ~>> (est + n div est, 0w1)
               in
                  if est' >= est then
                     est
                  else
                     loop est'
               end
         in
            if n < 2 then
               if n < 0 then
                  raise Domain
               else
                  n
            else
               (* By the above, this works provided n > n div 2 > 0, which follows from n >= 2. *)
               loop (~>> (n, 0w1))
         end


      fun factor2 x =
         let
            fun loop acc x =
               if andb (x, 1) = 0 then
                  loop (acc+1) (~>> (x, 0w1))
               else
                  (x, acc)
         in
            if x < 1 then
               raise Domain
            else
               loop 0 x
         end


      fun invmod (x, p) =
         let
            fun loop (acca, accb, x, y) =
               let
                  val (q, r) = divMod (x, y)
               in
                  if r = 0 then
                     if y = 1 then
                        accb
                     else
                        raise Div
                  else
                     loop (accb, (acca - accb * q) mod p, y, r)
               end
         in
            if p < 2 then
               raise Domain
            else if x < 0 then
               p - loop (0, 1, p, ~x)
            else
               loop (0, 1, p, x)
         end


      (* Takes 0^0 to be 1. *)
      fun powmod (y, x, p) =
         let
            (* if x <= 0 then returns (y^x * z) mod p *)
            fun loop y x z =
               if x = 0 then
                  z
               else if andb (x, 1) = 0 then
                  loop ((y * y) mod p) (~>> (x, 0w1)) z
               else
                  loop ((y * y) mod p) (~>> (x, 0w1)) ((z * y) mod p)
         in
            if p < 2 then
               raise Domain
            else if x > 0 then
               loop y x 1
            else if x = 0 then
               1
            else
               loop (invmod (y, p)) (~x) 1
         end


      fun isprime x =
         let
            val m = x - 1
            val (d, s) = factor2 m

            (* Miller-Rabin test *)
            fun maybePrime () =
               let
                  val a = Rand.randIntInf m + 1
                  
                  val atod = powmod (a, d, x)

                  fun loop i atod2i =
                     if i >= s then
                        false
                     else
                        atod2i = m
                        orelse
                        loop (i+1) ((atod2i * atod2i) mod x)
                        
               in
                  atod = 1  orelse  loop 0 atod
               end
                  
            fun tries n =
               if n = 0 then
                  true
               else
                  maybePrime ()
                  andalso
                  tries (n-1)
         in
            if x < 2 then
               raise Domain
            else
               (* 1 - (1/4)^10 = 99.9999% probability *)
               tries 10
         end


      (* p is a prime > 2 *)
      fun legendre (n, p) = powmod (n, (p-1) div 2, p)

      exception NotSquare

      fun sqrtmod (x, p) =
         if p <= 2 then
            if p = 2 then
               x
            else
               (* We aren't going to check that p is prime, but we'll do this easy case. *)
               raise Domain
         else 
            (case legendre (x, p) of
                0 => 0
                   
              | 1 =>
                   if p mod 4 = 3 then
                      powmod (x, (p+1) div 4, p)
                   else
                      let
                         val (q, s) = factor2 (p-1)

                         fun loopz () =
                            let
                               val z = Rand.randIntInf (p-3) + 2
                            in
                               if legendre (z, p) = p-1 then
                                  z
                               else
                                  loopz ()
                            end

                         val z = loopz ()

                         fun loop (c, r, t, m) =
                            if t = 1 then
                               r
                            else
                               let
                                  fun loopsq i texp =
                                     if texp = 1 then
                                        i
                                     else
                                        loopsq (Int.+ (i, 1)) ((texp * texp) mod p)

                                  val i = loopsq 0 t

                                  val b = powmod (c, << (1, Word.fromInt (Int.- (Int.- (m, i), 1))), p)
                                  val bsq = (b * b) mod p
                               in
                                  loop (bsq,
                                        (r * b) mod p,
                                        (t * bsq) mod p,
                                        i)
                               end
                      in
                         loop (powmod (z, q, p),
                               powmod (x, (q+1) div 2, p),
                               powmod (x, q, p),
                               IntInf.toInt s)
                      end

              | _ => raise NotSquare)

   end


structure Arith = ArithFun (structure Rand = MTRand)

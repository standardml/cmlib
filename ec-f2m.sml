
(* This is egregiously slow.  If we ever want to use this for something,
   the field operations (at least) will need to be rewritten.
*)

structure FieldF2m
   :>
   EC_FIELD
   where type index = int * IntInf.int
   where type elem = IntInf.int
   =
   struct

      open IntInf

      type index = Int.int * IntInf.int
      (* We index these fields by the pair of m (the maximum degree + 1) and their reduction polynomial,
         where a_m-1 x^m-1 + ... + a_1 x + a_0 is represented as the bitstring a_m ... a_1 a_0
      *)

      type elem = IntInf.int  (* Really shoud be WordInf, were there such a thing. *)

      fun validIndex (m, poly) =
         Int.> (m, 0)
         andalso
         poly > 0
         andalso
         IntInf.log2 poly = m
         (* We also ought to check that poly is irreducible. *)

      fun validElem ((m, _), x) =
         x = 0 orelse Int.> (m, IntInf.log2 x)

      fun size (m, _) = IntInf.<< (1, Word.fromInt m)

      val eq : elem * elem -> bool = op =

      fun zero _ = 0 : elem
      fun one _ = 1 : elem
      
      fun plus (_, x, y) = xorb (x, y)

      val minus = plus

      fun negate (_, x) = x

      fun times ((m, poly), x, y) =
         let
            val hi = << (1, Word.fromInt m)

            (* compute x * y + acc *)
            fun loop (x, y, acc) =
               if x = 0 then
                  acc
               else
                  let
                     val acc' =
                        if andb (x, 1) = 0 then
                           acc
                        else
                           xorb (acc, y)

                     val y' = << (y, 0w1)

                     val y'' =
                        if andb (y', hi) = 0 then
                           y'
                        else
                           xorb (y', poly)

                  in
                     loop (~>> (x, 0w1), y'', acc')
                  end
         in
            loop (x, y, 0)
         end

      fun polymult (x, y) =
         let
            (* a * b + c *)
            fun loop (a, b, c) =
               if a = 0 then
                  c
               else if andb (a, 1) = 0 then
                  loop (~>> (a, 0w1), << (b, 0w1), c)
               else
                  loop (~>> (a, 0w1), << (b, 0w1), xorb (b, c))
         in
            loop (x, y, 0)
         end

      fun polydivmod (dividend, divisor) =
         if dividend = 0 then
            (0, 0)
         else
            let
               val m = IntInf.log2 divisor
               val shift = Int.- (IntInf.log2 dividend, m)
   
               fun loop (x, shdiv, hi, sh, acc) =
                  let
                     val (x', acc') =
                        if andb (x, hi) = 0 then
                           (x, acc)
                        else
                           (xorb (x, shdiv), orb (acc, sh))
                  in
                     if sh = 1 then
                        (acc', x')
                     else
                        loop (x', IntInf.~>> (shdiv, 0w1), IntInf.~>> (hi, 0w1), IntInf.~>> (sh, 0w1), acc')
                  end
   
            in
               if Int.< (shift, 0) then
                  (0, dividend)
               else
                  loop (dividend, 
                        << (divisor, Word.fromInt shift), 
                        << (1, Word.fromInt (Int.+ (shift, m))),
                        << (1, Word.fromInt shift), 0)
            end


      fun inverse ((m, poly), x) =
         let
            fun loop (acca, accb, x, y) =
               let
                  val (q, r) = polydivmod (x, y)
               in
                  if r = 0 then
                     if y = 1 then
                        #2 (polydivmod (accb, poly))
                     else
                        raise Div
                  else
                     loop (accb, xorb (acca, polymult (accb, q)), y, r)
               end
         in
            loop (0, 1, poly, x)
         end

      fun elemToBytes ((m, _), x) = ConvertIntInf.toFixedBytesB (Int.div (Int.+ (m, 7), 8), x)

      val elemFromBytes = ConvertIntInf.fromBytesB

      fun elemToInt x = x

   end


(* Cryptographic randomness not required. *)
functor EllipticCurveF2mFun (structure InsecureRand : RAND)
   :>
   ELLIPTIC_CURVE
   where type Field.index = int * IntInf.int
   where type Field.elem = IntInf.int
   =
   struct

      open IntInf

      structure Field = FieldF2m

      structure F = Field
      structure InsecureRand = MTRand
      val xorb = IntInf.xorb

      type curve = {index : Field.index, a : Field.elem, b : Field.elem}
      type point = (Field.elem * Field.elem) option

      fun validCurve {index, a, b} =
         Field.validIndex index
         andalso
         Field.validElem (index, a)
         andalso
         Field.validElem (index, b)
         andalso
         not (F.eq (b, 0))

      fun validPoint ({index=idx, a, b}, pt) =
         (case pt of
             NONE => true
           | SOME (x, y) =>
                F.eq (F.times (idx, y, xorb (y, x)),
                      xorb (F.times (idx, F.times (idx, x, x),
                                     xorb (x, a)),
                            b)))

      val infinity : point = NONE

      fun double (curve as {index=idx, a, b}, pt) =
         (case pt of
             NONE => NONE
           | SOME (x, y) =>
                let
                   val xsq = F.times (idx, x, x)

                   val x' = xorb (xsq, F.times (idx, b, F.inverse (idx, xsq)))

                   val y' = xorb (xsq,
                                  F.times (idx,
                                           xorb (F.times (idx, y, F.inverse (idx, x)),
                                                 xorb (x, 1)),
                                           x'))
                in
                   SOME (x', y')
                end)

      fun plus (curve as {index=idx, a, b}, pt1, pt2) =
         (case pt1 of
             NONE => pt2
           | SOME (x1, y1) =>
                (case pt2 of
                    NONE => pt1
                  | SOME (x2, y2) =>
                       if x1 = x2 then
                          if y1 = y2 then
                             double (curve, pt1)
                          else
                             NONE
                       else
                          let
                             val m = F.times (idx,
                                              xorb (y1, y2),
                                              F.inverse (idx, xorb (x1, x2)))

                             val x3 = xorb (F.times (idx, m, m),
                                            xorb (m,
                                                  xorb (x1, xorb (x2, a))))

                             val y3 = xorb (F.times (idx, m, xorb (x1, x3)),
                                            xorb (x3, y1))
                          in
                             SOME (x3, y3)
                          end))

      fun negate ({index=p, ...}:curve, pt) =
         (case pt of
             NONE => NONE
           | SOME (x, y) => SOME (x, xorb (x, y)))

      fun eq (pt1, pt2) =
         (case (pt1, pt2) of
             (NONE, NONE) => true
           | (NONE, SOME _) => false
           | (SOME _, NONE) => false
           | (SOME (x1, y1), SOME (x2, y2)) =>
                Field.eq (x1, x2) andalso Field.eq (y1, y2))


      fun parity ({index=idx, ...}:curve, pt) =
         (case pt of
             NONE =>
                raise Domain
           | SOME (x, y) =>
                if x = 0 then
                   (* Standard arbitrarily assigns this even parity. *)
                   false
                else
                   andb (F.times (idx, y, F.inverse (idx, x)), 1) = 1)

      fun sqrt (idx as (m, _), x) =
         let
            fun loop (y, i) =
               if i = 0 then
                  y
               else
                  loop (F.times (idx, y, y), Int.- (i, 1))
         in
            loop (x, Int.- (m, 1))
         end
      
      (* for c <> 0, computes z such that z^2 + z + c = 0 *)
      fun quadraticRoot (idx as (m, _), c) =
         let
            fun loop () =
               let
                  (* Don't need cryptographic randomness here. This is just a randomized
                     algorithm, not key or nonce generation.
                  *)
                  val t = InsecureRand.randBits m
      
                  fun innerloop (z, w, i) =
                     if i = 0 then
                        (z, w)
                     else
                        let
                           val wsq = F.times (idx, w, w)
                           val z' = xorb (F.times (idx, z, z), F.times (idx, wsq, t))
                           val w' = xorb (wsq, c)
                        in
                           innerloop (z', w', Int.- (i, 1))
                        end
      
                  val (zfin, wfin) = innerloop (0, c, Int.- (m, 1))
               in
                  if wfin <> 0 then
                     raise Arith.NotSquare
                  else if xorb (F.times (idx, zfin, zfin), zfin) = 0 then
                     loop ()
                  else
                     zfin
               end
         in
            if c = 0 then
               0
            else
               loop ()
         end

      fun recoverPoint ({index=idx, a, b}, x, odd) =
         if x = 0 then
            SOME (SOME (x, sqrt (idx, b)))
         else
            let
               (* solve z^2 + z = x + a + bx^-2 *)
               val z =
                  quadraticRoot
                  (idx, xorb (xorb (x, a), F.times (idx, b, F.inverse (idx, F.times (idx, x, x)))))

               val z' =
                  if (andb (z, 1) <> 0) = odd then
                     z
                  else
                     xorb (z, 1)

               (* z' is another solution, since (z+1)^2 + (z+1) = z^2 + 1 + z + 1 = z^2 + z *)
            in
               (* Let  y = xz'.
                  Then y^2 + xy = x^2 z'^2 + x^2 z'
                                = x^2 (z'^2 + z')
                                = x^2 (x + a + bx^-2)
                                = x^3 + ax^2 + b
               *)
               SOME (SOME (x, F.times (idx, x, z')))
            end
            handle Arith.NotSquare => NONE

   end


structure EllipticCurveF2m = EllipticCurveF2mFun (structure InsecureRand = MTRand)

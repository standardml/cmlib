
structure FieldFp
   :>
   EC_FIELD
   where type index = IntInf.int
   where type elem = IntInf.int
   =
   struct

      open IntInf

      type index = IntInf.int
      type elem = IntInf.int

      fun validIndex p =
         p > 1
         andalso
         Arith.isprime p

      fun validElem (p, x) =
         x >= 0 andalso x < p

      fun size p = p

      val eq : elem * elem -> bool = op =

      fun zero _ = 0 : elem
      fun one _ = 1 : elem

      fun plus (p, x, y) = (x + y) mod p

      fun minus (p, x, y) = (x - y) mod p

      fun negate (p, x) = p - x
      
      fun times (p, x, y) = (x * y) mod p

      fun inverse (p, x) = Arith.invmod (x, p)

      fun elemToBytes (p, x) = ConvertIntInf.toFixedBytesB (Int.+ (Int.div (IntInf.log2 p, 8), 1), x)

      val elemFromBytes = ConvertIntInf.fromBytesB

      fun elemToInt x = x

   end


structure EllipticCurveFp
   :>
   ELLIPTIC_CURVE
   where type Field.index = IntInf.int
   where type Field.elem = IntInf.int
   =
   struct

      open IntInf

      structure Field = FieldFp

      fun inv (p, x) = Arith.invmod (x, p)

      type curve = {index : Field.index, a : Field.elem, b : Field.elem}
      type point = (Field.elem * Field.elem) option

      fun validCurve {index=p, a, b} =
         Field.validIndex p
         andalso
         a >= 0 andalso a < p
         andalso
         b > 0 andalso b < p
         andalso
         (((a * a) mod p * a * 4) mod p + (b * b * 27) mod p) mod p <> 0

      fun validPoint ({index=p, a, b}, pt) =
         (case pt of
             NONE => true
           | SOME (x, y) =>
                x > 0 andalso x < p
                andalso
                y > 0 andalso y < p
                andalso
                (((x * x) mod p * x) mod p - (y * y) mod p + (a * x) mod p + b) mod p = 0)

      val infinity : point = NONE

      fun double (curve as {index=p, a, b}, pt) =
         (case pt of
             NONE => NONE
           | SOME (x, y) =>
                let
                   val m = ((3 * x * x + a) mod p * inv (p, 2 * y)) mod p
                   
                   val x' = ((m * m) mod p - 2 * x) mod p

                   val y' = (m * (x - x') - y) mod p
                  
                in
                   SOME (x', y')
                end)

      fun plus (curve as {index=p, a, b}, pt1, pt2) =
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
                             val m = ((y2 - y1) * inv (p, (x2 - x1) mod p)) mod p
   
                             val x3 = ((m * m) mod p - x1 - x2) mod p
   
                             val y3 = (m * (x1 - x3) - y1) mod p
                          in
                             SOME (x3, y3)
                          end))

      fun negate ({index=p, ...}:curve, pt) =
         (case pt of
             NONE => NONE
           | SOME (x, y) => SOME (x, p-y))

      fun eq (pt1, pt2) =
         (case (pt1, pt2) of
             (NONE, NONE) => true
           | (NONE, SOME _) => false
           | (SOME _, NONE) => false
           | (SOME (x1, y1), SOME (x2, y2)) =>
                Field.eq (x1, x2) andalso Field.eq (y1, y2))


      fun parity (_, pt) =
         (case pt of
             NONE =>
                raise Domain
           | SOME (_, y) =>
                andb (y, 1) = 1)

      fun recoverPoint ({index=p, a, b}, x, odd) =
         let
            val y = Arith.sqrtmod ((((x * x) mod p * x) mod p + (a * x) mod p + b) mod p, p)
            val isodd = andb (y, 1) = 1
         in
            if odd = isodd then
               SOME (SOME (x, y))
            else
               SOME (SOME (x, p-y))
         end
         handle Arith.NotSquare => NONE

   end


functor EllipticCurveCryptoFun (structure EllipticCurve : ELLIPTIC_CURVE
                                structure SecureRandom : RANDOM)
   :>
   ELLIPTIC_CURVE_CRYPTO
   where type EC.Field.index = EllipticCurve.Field.index
   where type EC.Field.elem = EllipticCurve.Field.elem
   =
   struct

      structure EC = EllipticCurve
      structure R = RandFromRandom (structure Random = SecureRandom)

      open IntInf


      fun mult (curve, n, pt) =
         let
            (* returns mX + acc *)
            fun loop m X acc =
               if m = 0 then
                  acc
               else if andb (m, 1) = 0 then
                  loop (~>> (m, 0w1)) (EC.double (curve, X)) acc
               else
                  loop (~>> (m, 0w1)) (EC.double (curve, X)) (EC.plus (curve, acc, X))
         in
            loop n pt EC.infinity
         end


      type param = { curve : EC.curve,
                     base : EC.point,
                     order : IntInf.int,
                     cofactor : IntInf.int }

      type privkey = IntInf.int
      type pubkey = EC.point


      (* From "The Elliptic Curve Digital Signature Algorithm (ECDSA)" (Johnson, et al.), pp 23-24 *)
      fun validParam ({curve, base, order, cofactor}:param) =
         EC.validCurve curve
         andalso
         not (EC.eq (base, EC.infinity))
         andalso
         EC.validPoint (curve, base)
         andalso
         Arith.isprime order
         andalso
         Int.> (IntInf.log2 (order-1), 160)
         andalso
         let
            val q = EC.Field.size (#index curve)
         in
            (* order > 4 sqrt(q) *)
            order * order > 16 * q
            andalso
            EC.eq (mult (curve, order, base), EC.infinity)
            andalso
            let
               val z = q + Arith.sqrt (<< (q, 0w2)) + 1 - order * cofactor

               fun loop i =
                  Int.> (i, 20)
                  orelse
                  ((IntInf.pow (q, i) - 1) mod order <> 0
                   andalso loop (Int.+ (i, 1)))
            in
               (* Check that floor( (sqrt(q) + 1)^2 / order ) = cofactor.
                  let order=n, cofactor=h
                  floor( (sqrt(q) + 1)^2 / n ) = h
                  iff
                  (sqrt(q) + 1)^2 / n - 1 < h <= (sqrt(q) + 1)^2 / n
                  iff
                  (sqrt(q) + 1)^2 - n < nh <= (sqrt(q) + 1)^2

                  nh <= (sqrt(q) + 1)^2 
                      = q + 2 sqrt(q) + 1
                      = q + sqrt(4q) + 1
                  iff
                  q + sqrt(4q) + 1 - nh >= 0
                  iff
                  q + floor(sqrt(4q)) + 1 - nh >= 0  (since q and nh are integers)

                  nh > (sqrt(q) + 1)^2 - n
                  iff
                  nh + n > (sqrt(q) + 1)^2
                         = q + 2 sqrt(q) + 1
                         = q + sqrt(4q) + 1
                  iff
                  q + sqrt(4q) + 1 - nh < n
                  iff
                  q + floor(sqrt(4q)) + 1 - nh < n  (since q and nh are integers)
               *)
               z >= 0
               andalso
               z < order
               andalso
               loop 1
               andalso
               order <> q
            end
         end
                

      fun validPubkey ({curve, base, order, ...}:param, pt) =
         EC.validPoint (curve, pt)
         andalso
         not (EC.eq (pt, EC.infinity))
         andalso
         EC.eq (mult (curve, order, pt), EC.infinity)
         

      fun validPrivkey ({order, ...}:param, d) =
         d > 0
         andalso
         d < order


      fun newkey ({curve, base, order, ...}:param) =
         let
            val d = R.randIntInf (order-1) + 1
         in
            (mult (curve, d, base), d)
         end


      fun privkeyToPubkey ({curve, base, ...}:param, privkey) =
         mult (curve, privkey, base)

   end


structure EllipticCurveCryptoFp =
   EllipticCurveCryptoFun
   (structure EllipticCurve = EllipticCurveFp
    structure SecureRandom = AESFortuna)

structure EllipticCurveCryptoF2m =
   EllipticCurveCryptoFun
   (structure EllipticCurve = EllipticCurveF2m
    structure SecureRandom = AESFortuna)

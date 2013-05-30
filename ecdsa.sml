
functor ECDSAFun (structure EllipticCurveCrypto : ELLIPTIC_CURVE_CRYPTO
                  structure SecureRandom : RANDOM)
   :>
   ECDSA
   where type EC.Field.index = EllipticCurveCrypto.EC.Field.index
   where type EC.Field.elem = EllipticCurveCrypto.EC.Field.elem
   =
   struct

      structure R = RandFromRandom (structure Random = SecureRandom)

      open EllipticCurveCrypto
      open IntInf


      type sg = IntInf.int * IntInf.int


      fun sign (args as ({curve, base, order, ...}:param, privkey, msg)) =
         let
            val e = ConvertIntInf.fromBytesB msg

            (* Make the nonce depend on the message, to be more robust in case
               of bad random-number generation.  If the random number turns out
               to be predictable, you still only get the same nonce for the same
               message, which is harmless.  We don't use only the message, in case
               the hash used to generate the message is weak.
            *)
            val k =
               xorb (R.randBits (Int.+ (log2 order, 1)), e) mod (order-1) + 1
         in
            (case mult (curve, k, base) of
                NONE =>
                   (* If the parameters are correct, this can't happen. *)
                   raise (Fail "Bad parameters")
              | SOME (x, _) =>
                   let
                      val r = (EC.Field.elemToInt x) mod order
                   in
                      if r = 0 then
                         sign args
                      else
                         let
                            val t = Arith.invmod (k, order)
                            val s = (t * (e + (privkey * r) mod order)) mod order
                         in
                            if s = 0 then
                               sign args
                            else
                               (r, s)
                         end
                   end)
         end
                            

      fun verify ({curve, base, order, ...}:param, pubkey, msg, (r, s)) =
         r >= 1 andalso r < order
         andalso
         s >= 1 andalso s < order
         andalso
         let
            val e = ConvertIntInf.fromBytesB msg
            val w = Arith.invmod (s, order)
            val u1 = (e * w) mod order
            val u2 = (r * w) mod order
         in
            (* It would be cheaper to do both multiplies at once. *)
            (case EC.plus (curve, mult (curve, u1, base), mult (curve, u2, pubkey)) of
                NONE => false
              | SOME (x, _) =>
                   r = (EC.Field.elemToInt x) mod order)
         end

   end


structure ECDSAp =
   ECDSAFun
   (structure EllipticCurveCrypto = EllipticCurveCryptoFp
    structure SecureRandom = AESFortuna)

structure ECDSA2m =
   ECDSAFun 
   (structure EllipticCurveCrypto = EllipticCurveCryptoF2m
    structure SecureRandom = AESFortuna)
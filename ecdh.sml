
functor ECDHFun (structure EllipticCurveCrypto : ELLIPTIC_CURVE_CRYPTO)
   :>
   ECDH
   where type EC.Field.index = EllipticCurveCrypto.EC.Field.index
   where type EC.Field.elem = EllipticCurveCrypto.EC.Field.elem
   =
   struct

      open EllipticCurveCrypto
      
      type secret = IntInf.int

      fun secret ({curve, ...}:param, pubkey, privkey) =
         (case mult (curve, privkey, pubkey) of
             NONE => 
                (* If the parameters are correct, this can't happen. *)
                raise (Fail "Bad parameters")
           | SOME (x, _) =>
                EC.Field.elemToInt x)

   end


structure ECDHp = ECDHFun (structure EllipticCurveCrypto = EllipticCurveCryptoFp)
structure ECDH2m = ECDHFun (structure EllipticCurveCrypto = EllipticCurveCryptoF2m)

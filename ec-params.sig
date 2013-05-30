
signature ELLIPTIC_CURVE_PARAMS =
   sig

      type paramp = EllipticCurveCryptoFp.param
      type param2m = EllipticCurveCryptoF2m.param

      val secp256k1 : paramp
      val secp256r1 : paramp

      val sect283k1 : param2m
      val sect283r1 : param2m

   end


signature ELLIPTIC_CURVE_CRYPTO =
   sig

      structure EC : ELLIPTIC_CURVE

      val mult : EC.curve * IntInf.int * EC.point -> EC.point

      type param = { curve : EC.curve,
                     base : EC.point,
                     order : IntInf.int,
                     cofactor : IntInf.int }

      type privkey = IntInf.int
      type pubkey = EC.point

      val validParam : param -> bool
      val validPrivkey : param * privkey -> bool
      val validPubkey : param * pubkey -> bool

      val newkey : param -> pubkey * privkey

      val privkeyToPubkey : param * privkey -> pubkey

   end


signature ECDSA =
   sig

      structure EC : ELLIPTIC_CURVE

      include DIGITAL_SIGNATURE
              where type param = { curve : EC.curve,
                                   base : EC.point,
                                   order : IntInf.int,
                                   cofactor : IntInf.int }
              where type privkey = IntInf.int
              where type pubkey = EC.point
              where type sg = IntInf.int * IntInf.int

      val privkeyToPubkey : param * privkey -> pubkey

   end

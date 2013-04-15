
signature ECDH =
   sig

      structure EC : ELLIPTIC_CURVE

      include SHARED_SECRET
              where type param = { curve : EC.curve,
                                   base : EC.point,
                                   order : IntInf.int,
                                   cofactor : IntInf.int }
              where type privkey = IntInf.int
              where type pubkey = EC.point
              where type secret = IntInf.int

      val privkeyToPubkey : param * privkey -> pubkey

   end

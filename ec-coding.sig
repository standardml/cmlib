
signature ELLIPTIC_CURVE_CODING =
   sig

      type data

      structure EC : ELLIPTIC_CURVE

      type param = { curve : EC.curve,
                     base : EC.point,
                     order : IntInf.int,
                     cofactor : IntInf.int }
      type privkey = IntInf.int
      type pubkey = EC.point
      type sg = IntInf.int * IntInf.int

      exception Invalid

      val encodePubkey : param * pubkey -> Bytestring.string
      val encodePubkeyCompressed : param * pubkey -> Bytestring.string
      val decodePubkey : param * Bytestring.string -> pubkey

      val encodePrivkey : privkey -> Bytestring.string
      val decodePrivkey : Bytestring.string -> privkey

      val encodeKeypair : param * pubkey * privkey -> Bytestring.string
      val decodeKeypair : param * Bytestring.string -> pubkey * privkey

      val encodeSg : sg -> Bytestring.string
      val decodeSg : Bytestring.string -> sg

   end

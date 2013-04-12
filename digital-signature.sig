
signature DIGITAL_SIGNATURE =
   sig

      type param
      type privkey
      type pubkey
      type sg

      val validParam : param -> bool
      val validPrivkey : param * privkey -> bool
      val validPubkey : param * pubkey -> bool

      val newkey : param -> pubkey * privkey
      val sign : param * privkey * Bytestring.string -> sg
      val verify : param * pubkey * Bytestring.string * sg -> bool

   end

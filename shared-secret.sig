
signature SHARED_SECRET =
   sig

      type param
      type privkey
      type pubkey
      type secret

      val validParam : param -> bool
      val validPrivkey : param * privkey -> bool
      val validPubkey : param * pubkey -> bool

      val newkey : param -> pubkey * privkey
      val secret : param * pubkey * privkey -> secret

   end

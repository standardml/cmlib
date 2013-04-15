
signature CIPHER =
   sig

      type prekey
      type init
      type key

      val messageSize : int option

      val makeKey : prekey * init -> key
      val encrypt : key * Bytestring.string -> Bytestring.string
      val decrypt : key * Bytestring.string -> Bytestring.string

   end

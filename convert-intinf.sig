
signature CONVERT_INT_INF =
   sig

      val toBytesB : IntInf.int -> Bytestring.string
      val toBytesL : IntInf.int -> Bytestring.string
      val toFixedBytesB : int * IntInf.int -> Bytestring.string
      val toFixedBytesL : int * IntInf.int -> Bytestring.string
      val fromBytesB : Bytestring.string -> IntInf.int
      val fromBytesL : Bytestring.string -> IntInf.int
      val fromSignedBytesB : bool * Bytestring.string -> IntInf.int
      val fromSignedBytesL : bool * Bytestring.string -> IntInf.int

   end

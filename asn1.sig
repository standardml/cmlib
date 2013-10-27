
(* An incomplete collection of ASN1 combinators. *)

signature ASN1 =
   sig

      type data
      type 'a format

      datatype decodeError =
         ILLEGAL of int
       | MISMATCH of int
       | FIXED

      exception IllegalFormat
      exception EncodeError
      exception DecodeError of decodeError

      val map : ('a -> 'b) -> ('b -> 'a) -> 'a format -> 'b format

      val tuple2 : 'a format * 'b format -> ('a * 'b) format
      val tuple3 : 'a format * 'b format * 'c format -> ('a * 'b * 'c) format
      val tuple4 : 'a format * 'b format * 'c format * 'd format -> ('a * 'b * 'c * 'd) format
      val tuple5 : 'a format * 'b format * 'c format * 'd format * 'e format -> ('a * 'b * 'c * 'd * 'e) format
      val tuple6 : 'a format * 'b format * 'c format * 'd format * 'e format * 'f format -> ('a * 'b * 'c * 'd * 'e * 'f) format
      val option : 'a format -> 'a option format
      val union : 'a format * 'b format -> ('a, 'b) Sum.sum format
      val tag : Word8.word * 'a format -> 'a format

      val integer : IntInf.int format
      val unsigned : IntInf.int format
      val bytestring : Bytestring.string format
      val objectid : int list format
      val null : unit format
      val any : data format

      (* a bitstring is represented as (padded bitstring, number of extra bits) *)
      val bitstring : (Bytestring.string * int) format

      val omit : unit format
      val fixed : ''a * ''a format -> unit format

      val encode : 'a format -> 'a -> data
      val decode : 'a format -> data -> 'a

   end

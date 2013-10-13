
(* A substring-like interface for Word8VectorSlice. *)

signature BYTESUBSTRING =
   sig

      type substring = Word8VectorSlice.slice
      type string = Bytestring.string
      type byte = Word8.word
      type char = byte

      val sub : substring * int -> byte
      val size : substring -> int
      val base : substring -> string * int * int
      val extract : string * int * int option -> substring
      val substring : string * int * int -> substring
      val full : string -> substring
      val string : substring -> string
      val isEmpty : substring -> bool
      val getc : substring -> (byte * substring) option
      val slice : substring * int * int option -> substring
      val concat : substring list -> string
      val null : substring
      val explode : substring -> byte list
      val splitAt : substring * int -> substring * substring
      val map : (byte -> byte) -> substring -> string
      val map2 : (byte * byte -> byte) -> substring * substring -> string
      val rev : substring -> string
      val eq : substring * substring -> bool
      val compare : substring * substring -> order

      val fromWord8Slice : Word8VectorSlice.slice -> substring

      val toWord8Slice : substring -> Word8VectorSlice.slice
      val toString : substring -> String.string
      val toStringHex : substring -> String.string
      val toStringHex' : String.string -> substring -> String.string

   end

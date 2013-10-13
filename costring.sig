
(* A stream with string-like operations. *)

signature COSTRING =
   sig

      type elem
      type string
      type costring
   
      structure Streamable :
         MONO_STREAMABLE
         where type t = costring
         where type elem = elem

      val full : string -> costring
      val null : costring
      val fromStream : string Stream.stream -> costring

      val minSize : costring * int -> bool
      val maxSize : costring * int -> bool

      val getc : costring -> (elem * costring) option
      val sub : costring * int -> elem
      val slice : costring * int * int -> string
      val suffix : costring * int -> costring
      val splitAt : costring * int -> string * costring

      val all : costring -> string

   end

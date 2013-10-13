
structure StringMiniString
   :>
   MINI_STRING 
   where type string = string
   where type elem = char
   =
   struct
      open String
      type elem = char
      val null = ""
   end


structure BytestringMiniString
   :>
   MINI_STRING 
   where type string = Bytestring.string
   where type elem = Word8.word
   =
   struct
      open Bytestring
      type elem = Word8.word
   end


structure SubstringMiniString
   :>
   MINI_STRING
   where type string = substring
   where type elem = char
   =
   struct
      structure S = Substring

      type string = S.substring
      type elem = char

      val sub = S.sub
      val size = S.size
      val extract = S.slice
      val explode = S.explode

      fun concat l = S.full (S.concat l)
      fun implode l = S.full (String.implode l)
      val null = S.full ""
   end


structure BytesubstringMiniString
   :>
   MINI_STRING
   where type string = Bytesubstring.substring
   where type elem = Word8.word
   =
   struct
      structure S = Bytesubstring

      type string = S.substring
      type elem = Word8.word

      val sub = S.sub
      val size = S.size
      val extract = S.slice
      val explode = S.explode

      fun concat l = S.full (S.concat l)
      fun implode l = S.full (Bytestring.implode l)
      val null = S.full Bytestring.null
   end

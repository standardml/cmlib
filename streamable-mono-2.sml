
structure SubstringMonoStreamable
   :> MONO_STREAMABLE
      where type t = Substring.substring
      where type elem = char
   =
   struct

      type t = Substring.substring
      type elem = char

      datatype front = Nil | Cons of elem * t

      fun front str =
         (case Substring.getc str of
             NONE => Nil
           | SOME (ch, str') => Cons (ch, str'))

   end


structure BytesubstringMonoStreamable
   :> MONO_STREAMABLE
      where type t = Bytesubstring.substring
      where type elem = Word8.word
   =
   struct

      type t = Bytesubstring.substring
      type elem = Word8.word

      datatype front = Nil | Cons of elem * t

      fun front str =
         (case Bytesubstring.getc str of
             NONE => Nil
           | SOME (ch, str') => Cons (ch, str'))

   end

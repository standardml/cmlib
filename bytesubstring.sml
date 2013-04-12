
structure Bytesubstring :> BYTESUBSTRING =
   struct

      type substring = Word8VectorSlice.slice
      type string = Bytestring.string
      type byte = Word8.word
      type char = byte

      structure V = Word8VectorSlice
      
      val tovec = Bytestring.toWord8Vector
      val fromvec = Bytestring.fromWord8Vector

      val size = V.length
      val sub = V.sub
      val isEmpty = V.isEmpty
      val getc = V.getItem
      val slice = V.subslice

      fun base s =
         let
            val (s', start, len) = V.base s
         in
            (fromvec s', start, len)
         end

      fun extract (s, start, leno) = V.slice (tovec s, start, leno)

      fun substring (s, start, len) = V.slice (tovec s, start, SOME len)

      fun full s = V.full (tovec s)

      fun string s = fromvec (V.vector s)

      fun concat l = fromvec (V.concat l)

      fun explode s = V.foldr (op ::) nil s

      fun splitAt (s, i) =
         (V.subslice (s, 0, SOME i),
          V.subslice (s, i, NONE))

      fun fromWord8Slice s = s
      fun toWord8Slice s = s

      fun eq (s1, s2) =
         let
            val len = size s1

            fun loop i =
               if i >= len then
                  true
               else
                  V.sub (s1, i) = V.sub (s2,i)  andalso  loop (i+1)
         in
            size s2 = len  andalso  loop 0
         end

      fun compare (s1, s2) =
         let
            val len1 = size s1
            val len2 = size s2

            fun loop i =
               if i >= len1 then
                  if i >= len2 then
                     EQUAL
                  else
                     LESS
               else
                  if i >= len2 then
                     GREATER
                  else
                     (case Word8.compare (V.sub (s1, i), V.sub (s2, i)) of
                         EQUAL =>
                            loop (i+1)
                       | x => x)
         in
            loop 0
         end



      fun toStringOrd s =
         String.implode (List.rev (V.foldl (fn (b, l) => Char.chr (Word8.toInt b) :: l) [] s))

      val ch0 = Char.ord #"0"
      val cha = Char.ord #"a" - 10

      fun nibblestr (n:byte) =
         if n <= 0w9 then
            String.str (Char.chr (Word8.toInt n + ch0))
         else
            String.str (Char.chr (Word8.toInt n + cha))

      fun toStringHex' sep s =
         if V.length s = 0 then
            ""
         else
            let
               val b = V.sub (s, 0)
            in
               String.concat
               (nibblestr (Word8.>> (b, 0w4))
                :: nibblestr (Word8.andb (b, 0wxf))
                :: V.foldr
                      (fn (b, l) =>
                          sep
                          :: nibblestr (Word8.>> (b, 0w4))
                          :: nibblestr (Word8.andb (b, 0wxf))
                          :: l)
                      []
                      (V.subslice (s, 1, NONE)))
            end

      val toStringHex = toStringHex' ""

   end


structure IntInfHashable
   :> HASHABLE where type t = IntInf.int
   =
   struct
      type t = IntInf.int

      val eq : IntInf.int * IntInf.int -> bool = op =

      val wordSize = Word.fromInt Word.wordSize
      val mask = IntInf.<< (1, wordSize) - 1

      fun hashloop n acc =
         if n = 0 then
            acc
         else
            hashloop 
               (IntInf.~>> (n, wordSize))
               (JenkinsHash.hashInc acc (ConvertWord.intInfToWord (IntInf.andb (n, mask))))

      fun hash n =
         if n >= 0 then
            hashloop n 0w0
         else
            Word.notb (hashloop n 0w0)

   end


structure Word8Hashable
   :> HASHABLE where type t = Word8.word
   =
   struct
      type t = Word8.word

      val eq : Word8.word * Word8.word -> bool = op =
      val hash = ConvertWord.word8ToWord
   end


structure BytestringHashable
   :> HASHABLE where type t = Bytestring.string
   =
   struct
      type t = Bytestring.string

      val eq = Bytestring.eq

      fun hash str =
          let
             val len = Bytestring.size str
                
             fun loop i h =
                 if i >= len then
                    h
                 else
                    loop (i+1) (JenkinsHash.hashInc h (ConvertWord.word8ToWord (Bytestring.sub (str, i))))
          in
             loop 0 0w0
          end
   end


functor SetHashable (structure Set : SET
                     structure Elem : HASHABLE
                     sharing type Set.elem = Elem.t)
   :> HASHABLE where type t = Set.set
   =
   struct

      type t = Set.set

      val eq = Set.eq

      fun hash set =
         Set.foldl
         (fn (elem, acc) => JenkinsHash.hashInc acc (Elem.hash elem))
         0w0
         set

   end

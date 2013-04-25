structure CharHashable
   :> HASHABLE where type t = char
   =
   struct
      type t = char

      val eq : char * char -> bool = op =

      fun hash ch = Word.fromInt (Char.ord ch)
   end


structure IntHashable
   :> HASHABLE where type t = int
   =
   struct
      type t = int

      val eq : int * int -> bool = op =
      val hash = Word.fromInt
   end


structure IntInfHashable
   :> HASHABLE where type t = IntInf.int
   =
   struct
      type t = IntInf.int

      val eq : IntInf.int * IntInf.int -> bool = op =
      val hash = Word.fromLargeInt
   end


structure WordHashable
   :> HASHABLE where type t = Word.word
   =
   struct
      type t = Word.word

      val eq : Word.word * Word.word -> bool = op =
      fun hash x = x
   end


structure Word8Hashable
   :> HASHABLE where type t = Word8.word
   =
   struct
      type t = Word8.word

      val eq : Word8.word * Word8.word -> bool = op =
      val hash = ConvertWord.word8ToWord
   end


structure StringHashable
   :> HASHABLE where type t = string
   =
   struct
      type t = string

      val eq : string * string -> bool = op =

      fun hash str =
          let
             val len = String.size str
                
             fun loop i h =
                 if i >= len then
                    h
                 else
                    loop (i+1) (JenkinsHash.hashInc h (Word.fromInt (Char.ord (String.sub (str, i)))))
          in
             loop 0 0w0
          end
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


structure UnitHashable
   :> HASHABLE where type t = unit
   =
   struct
      type t = unit

      fun eq _ = true
      fun hash _ = 0w0
   end


functor ListHashable (structure Elem : HASHABLE)
   :> HASHABLE where type t = Elem.t list
   =
   struct

      type t = Elem.t list

      fun eq l1_l2 =
         (case l1_l2 of
             ([], []) =>
                true
           | (h1 :: t1, h2 :: t2) =>
                Elem.eq (h1, h2)
                andalso
                eq (t1, t2)
           | _ =>
                false)

      fun hashLoop l acc =
         (case l of
             [] => acc
           | h :: t =>
                hashLoop t (JenkinsHash.hashInc acc (Elem.hash h)))

      fun hash l = hashLoop l 0w0

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

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


structure WordHashable
   :> HASHABLE where type t = Word.word
   =
   struct
      type t = Word.word

      val eq : Word.word * Word.word -> bool = op =
      fun hash x = x
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


functor ProductHashable (structure X : HASHABLE
                         structure Y : HASHABLE)
   :> HASHABLE where type t = X.t * Y.t
   =
   struct

      type t = X.t * Y.t

      fun eq ((x, y), (x', y')) =
         X.eq (x, x') andalso Y.eq (y, y')

      fun hash (x, y) =
         MJHash.hashInc (X.hash x) (Y.hash y)

   end


structure IntHashable
   :> HASHABLE where type t = int
   =
   struct
      type t = int

      val eq : int * int -> bool = op =
      val hash = Word.fromInt
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

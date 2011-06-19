
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

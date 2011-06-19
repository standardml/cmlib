
structure JenkinsHash
   :> HASH_INCREMENT
   =
   struct

      (* Jenkins hash function *)

      fun hashInc hash datum =
          let
             val hash = Word.+ (hash, datum)
             val hash = Word.+ (hash, Word.<< (hash, 0w10))
             val hash = Word.xorb (hash, Word.>> (hash, 0w6))
          in
             hash
          end

   end

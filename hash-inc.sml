
structure JenkinsHash :> HASH_INCREMENT
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


(* A non-commutative variant of the Jenkins hash. *)
structure MJHash :> HASH_INCREMENT
   =
   struct

      fun hashInc hash datum =
          let
             val hash = Word.+ (hash, Word.<< (hash, 0w10))
             val hash = Word.xorb (hash, Word.>> (hash, 0w6))
             val hash = Word.+ (hash, datum)
          in
             hash
          end

   end

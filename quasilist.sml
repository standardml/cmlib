
structure Quasilist :> QUASILIST =
   struct

      datatype 'a quasilist = 
         Zero
       | One of 'a
       | Append of 'a quasilist * 'a quasilist

      fun frontMain l1 l2 =
         (case l1 of
             Zero =>
                front l2
           | One x =>
                SOME (x, l2)
           | Append (l1a, l1b) =>
                frontMain l1a (Append (l1b, l2)))

      and front l =
         (case l of
             Zero =>
                NONE
           | One x =>
                SOME (x, Zero)
           | Append (l1, l2) =>
                frontMain l1 l2)

      fun foldl f base l =
         (case front l of
             NONE =>
                base
           | SOME (h, t) =>
                foldl f (f (h, base)) t)

      fun toList l = rev (foldl (op ::) [] l)

   end

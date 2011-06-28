
structure Partition =
   struct

      open UnionFind

      fun partition f l =
         let
            fun loop sets l =
               (case l of
                   nil =>
                      sets
                 | h :: t =>
                      let
                         val set = new (Quasilist.One h)
                         
                         val () =
                            app
                            (fn (x, set') =>
                                if f (h, x) then
                                   union Quasilist.Append set set'
                                else
                                   ())
                            sets
                      in
                         loop ((h, set) :: sets) t
                      end)

            val sets = loop [] l
         in
            foldl
            (fn ((_, set), ls) =>
                if isCanonical set then
                   Quasilist.toList (find set) :: ls
                else
                   ls)
            []
            sets
         end
         
   end

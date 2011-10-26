
functor PartitionFun (structure Sequence : SEQUENCE)
   :>
   PARTITION where type 'a seq = 'a Sequence.seq
   =
   struct

      open IUnionFind
      open Sequence

      fun partition f s =
         let
            fun loop sets s =
               (case showt s of
                   EMPTY =>
                      sets
                 | ELT x =>
                      let
                         val set = new (singleton x)
                         
                         val () =
                            app
                            (fn (y, set') =>
                                if sameSet (set, set') then
                                   ()
                                else if f (x, y) then
                                   union append set set'
                                else
                                   ())
                            sets
                      in
                         (x, set) :: sets
                      end
                 | NODE (left, right) =>
                      loop (loop sets left) right)

            val sets = loop [] s
         in
            List.foldl
            (fn ((_, set), ls) =>
                if isCanonical set then
                   cons (find set, ls)
                else
                   ls)
            (empty ())
            sets
         end
         
   end


functor TopologicalSort (structure Key : ORDERED
                         type value
                         val key : value -> Key.t
                         val deps : value -> Key.t list)
   :> TOPOLOGICAL_SORT where type key = Key.t where type value = value
   =
   struct

      type key = Key.t
      type value = value
      
      structure D = RedBlackDict (structure Key = Key)

      datatype color = WHITE | GRAY | BLACK

      exception Cycle of key

      fun search d (key, acc) =
         (case D.find d key of
             NONE => acc

           | SOME (color, edges, value) =>
                (case !color of
                    BLACK => acc

                  | GRAY =>
                       raise (Cycle key)

                  | WHITE =>
                       let
                          val () = color := GRAY
                          val acc' = List.foldl (search d) acc edges
                          val () = color := BLACK
                       in
                          value :: acc'
                       end))

      fun sort graph =
         let
            val d =
               List.foldl
               (fn (x, d) => D.insert d (key x) (ref WHITE, deps x, x))
               D.empty
               graph
         in
            rev (D.foldl (fn (key, _, acc) => search d (key, acc)) [] d)
         end
      
   end
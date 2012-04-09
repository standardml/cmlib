
structure CoordinatedStream
   :> COORDINATED_STREAM
      where type 'a stream = 'a Stream.stream
   =
   struct

      open Stream

      fun coordinate eoln coord s =
         lazy
         (fn () =>
             (case front s of
                 Nil => Nil
               | Cons (x, s') =>
                    let
                       val coord' =
                          if eoln s then
                             Coord.nextline coord
                          else
                             Coord.nextchar coord
                    in
                       Cons ((x, coord'), coordinate eoln coord' s')
                    end))

   end

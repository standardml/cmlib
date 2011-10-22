
signature COORDINATED_STREAM =
   sig

      type 'a stream
      val coordinate : ('a stream -> bool) -> Coord.coord -> 'a stream -> ('a * Coord.coord) stream

   end


functor MarkStreamFn (structure S : STREAMABLE
                      type tok
                      val eol: tok * tok S.t -> bool) = 
   struct
      (*[ val mark: 
             string -> tok S.t -> Coord.t * (tok * Coord.t) Stream.stream ]*)
      fun mark name stream = 
         let
            fun loop c stream () = 
               (case S.front stream of 
                   S.Nil => Stream.Nil
                 | S.Cons (tok, stream) =>
                   let
                      val c' = 
                         if eol (tok, stream) 
                         then Coord.nextline c
                         else Coord.nextchar c
                   in
                      Stream.Cons ((tok, c'), Stream.lazy (loop c' stream))
                   end)
            val c = Coord.init name
         in
            (c, Stream.lazy (loop c stream))
         end
   end 

(* Obeys the unicode standard for what is a newline (\n, \v, \f, \r, or \r\n)
 * over ASCII characters (see http://en.wikipedia.org/wiki/Newline#Unicode) *)
structure MarkCharStream = 
MarkStreamFn (structure S = StreamStreamable 
              type tok = char
              fun eol (#"\n", _) = true
                | eol (#"\v", _) = true
                | eol (#"\f", _) = true
                | eol (#"\r", stream) = 
                  (case Stream.front stream of
                      Stream.Cons (#"\n", _) => false
                    | _ => true)
                | eol _ = false )

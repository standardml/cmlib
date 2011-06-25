
functor LexEngineFun (type symbol
                      val ord : symbol -> int)
   :> LEX_ENGINE 
      where type symbol = symbol
   =
   struct

      type symbol = symbol

      (* Next state function for an 7-bit lexer, with 1-byte state numbers. *)
      fun next7x1 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 128 * state + symbol
             in                       
                Char.ord (String.sub (table, i))
             end

      fun next8x1 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 256 * state + symbol
             in                       
                Char.ord (String.sub (table, i))
             end

      fun next9x1 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 512 * state + symbol
             in                       
                Char.ord (String.sub (table, i))
             end

      fun next10x1 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 1024 * state + symbol
             in                       
                Char.ord (String.sub (table, i))
             end

      fun next7x2 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 256 * state + 2 * symbol
             in                       
                Char.ord (String.sub (table, i)) * 256 +
                Char.ord (String.sub (table, i+1))
             end

      fun next8x2 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 512 * state + 2 * symbol
             in                       
                Char.ord (String.sub (table, i)) * 256 +
                Char.ord (String.sub (table, i+1))
             end

      fun next9x2 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 1024 * state + 2 * symbol
             in                       
                Char.ord (String.sub (table, i)) * 256 +
                Char.ord (String.sub (table, i+1))
             end

      fun next10x2 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 2048 * state + 2 * symbol
             in                       
                Char.ord (String.sub (table, i)) * 256 +
                Char.ord (String.sub (table, i+1))
             end


      (* Next state function for end-of-stream, with 1-byte state numbers. *)
      fun next0x1 table state =
          Char.ord (String.sub (table, state))

      fun next0x2 table state =
          let
             val i = 2 * state
          in
             Char.ord (String.sub (table, i)) * 256 +
             Char.ord (String.sub (table, i+1))
          end


      type ('a, 'b) action = { str : symbol list,
                               len : int,
                               start : symbol Stream.stream,
                               follow : symbol Stream.stream,
                               self : 'b } -> 'a

      type ('a, 'b) table =
         int * int * int * ('a, 'b) action vector * (int -> int -> int) * (int -> int)

      fun lex self (initial, lastAcceptSink, lastAccept, acceptTable, next, nextEos) s =
          let
             fun loop candidate candLen candChars candStream state len chars s =
                if state = 0 then
                   (candidate, candLen, candChars, candStream)
                else if state <= lastAccept then
                   if state <= lastAcceptSink then
                      (state, len, chars, s)
                   else
                      (case Stream.front s of
                          Stream.Nil =>
                             loop state len chars s (nextEos state) len chars s
                        | Stream.Cons (ch, s') =>
                             loop state len chars s (next state (ord ch)) (len+1) (ch :: chars) s')
                else
                   (case Stream.front s of
                       Stream.Nil =>
                          loop candidate candLen candChars candStream (nextEos state) len chars s
                     | Stream.Cons (ch, s') =>
                          loop candidate candLen candChars candStream (next state (ord ch)) (len+1) (ch :: chars) s')
                            
             val (acceptingState, len, chars, s') = 
                (* By construction, initial is an accepting state.  It generally shouldn't be a
                   accepting-sink state (since it means the lexer is uninteresting), but that's
                   allowed, so we check for it.
                *)
                if initial <= lastAcceptSink then
                   (initial, 0, [], s)
                else
                   loop initial 0 [] s initial 0 [] s

             val f = Vector.sub (acceptTable, acceptingState-1)
          in
             f { str = rev chars,
                 len = len,
                 start = s,
                 follow = s',
                 self = self }
          end

   end

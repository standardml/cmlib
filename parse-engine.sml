
functor ParseEngineFun (structure Streamable : STREAMABLE
                        type terminal
                        type value
                        val read : terminal -> int * value
                        val dummyValue : value)
   :> PARSE_ENGINE where type terminal = terminal
                     and type value = value
   =
   struct

      structure Streamable = Streamable

      type terminal = terminal
      type value = value

      (* Transition function for a 5-bit symbols, with 1-byte results. *)
      fun next5x1 table state symbol =
         let
            val i = 32 * state + symbol
         in                       
            Char.ord (String.sub (table, i)) - 128
         end

      fun next6x1 table state symbol =
         let
            val i = 64 * state + symbol
         in                       
            Char.ord (String.sub (table, i)) - 128
         end

      fun next7x1 table state symbol =
         let
            val i = 128 * state + symbol
         in                       
            Char.ord (String.sub (table, i)) - 128
         end

      fun next8x1 table state symbol =
         let
            val i = 256 * state + symbol
         in                       
            Char.ord (String.sub (table, i)) - 128
         end

      fun next9x1 table state symbol =
         let
            val i = 512 * state + symbol
         in                       
            Char.ord (String.sub (table, i)) - 128
         end

      fun next5x2 table state symbol =
         let
            val i = 64 * state + 2 * symbol
         in                       
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end

      fun next6x2 table state symbol =
         let
            val i = 128 * state + 2 * symbol
         in                       
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end

      fun next7x2 table state symbol =
         let
            val i = 256 * state + 2 * symbol
         in                       
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end

      fun next8x2 table state symbol =
         let
            val i = 512 * state + 2 * symbol
         in                       
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end

      fun next9x2 table state symbol =
         let
            val i = 1024 * state + 2 * symbol
         in                       
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end


      type action = value list -> value list

      type table =
         (int -> int -> int)             (* action table *)
         *
         (int -> int -> int)             (* goto table *)
         *
         (int * int * action) vector     (* reduction information: lhs, size of rhs, functions to call *)
         *
         (terminal Streamable.t -> exn)  (* error function *)

      fun parse (action, goto, reduce, error) s =
         let
            fun loop ststack valstack s =
               (case Streamable.front s of
                   Streamable.Nil =>
                      loopRead ststack valstack 0 dummyValue s s
                 | Streamable.Cons (term, s') =>
                      let
                         val (ordinal, value) = read term
                      in
                         loopRead ststack valstack ordinal value s s'
                      end)

            and loopRead ststack valstack ordinal value s s' =
               (case ststack of
                   [] =>
                      raise (Fail "bad parsing table")
                 | state :: rest =>
                      let
                         val n = action state ordinal
                      in
                         if n = 0 then
                            raise (error s)
                         else if n > 0 then
                            (* shift *)
                            loop (n-1 :: ststack) (value :: valstack) s'
                         else if n = ~1 then
                            (* accept *)
                            (valstack, s)
                         else
                            (* reduce *)
                            let
                               val (lhs, rhssize, f) = Vector.sub (reduce, ~n - 2)
                               val ststack' = List.drop (ststack, rhssize)
                            in
                               loopRead (goto (List.hd ststack') lhs :: ststack') (f valstack) ordinal value s s'
                            end
                      end)

         in
            (case loop [0] [] s of
                ([value], s') => (value, s')
              | _ =>
                   raise (Fail "bad parsing table"))
         end
   end

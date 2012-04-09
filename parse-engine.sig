
signature PARSE_ENGINE =
   sig

      structure Streamable : STREAMABLE

      type terminal
      type value

      val next5x1 : string -> int -> int -> int
      val next6x1 : string -> int -> int -> int
      val next7x1 : string -> int -> int -> int
      val next8x1 : string -> int -> int -> int
      val next9x1 : string -> int -> int -> int
      val next5x2 : string -> int -> int -> int
      val next6x2 : string -> int -> int -> int
      val next7x2 : string -> int -> int -> int
      val next8x2 : string -> int -> int -> int
      val next9x2 : string -> int -> int -> int

      type action = value list -> value list

      type 'a table =
         (int -> int -> int)             (* action table *)
         *
         (int -> int -> int)             (* goto table *)
         *
         (int * int * action) vector     (* reduction information: lhs ordinal, rhs size, function *)
         *
         (value -> 'a)                   (* result destructor *)
         *
         (terminal Streamable.t -> exn)  (* error function *)

      val parse : 'a table -> terminal Streamable.t -> 'a * terminal Streamable.t

   end


signature LEX_ENGINE =
   sig

      structure Streamable : STREAMABLE

      type symbol

      val next7x1 : int -> string -> int -> int -> int
      val next8x1 : int -> string -> int -> int -> int
      val next9x1 : int -> string -> int -> int -> int
      val next10x1 : int -> string -> int -> int -> int
      val next7x2 : int -> string -> int -> int -> int
      val next8x2 : int -> string -> int -> int -> int
      val next9x2 : int -> string -> int -> int -> int
      val next10x2 : int -> string -> int -> int -> int

      val next0x1 : string -> int -> int
      val next0x2 : string -> int -> int

      type ('a, 'b) action =
         { match : symbol list,
           len : int,
           start : symbol Streamable.t,
           follow : symbol Streamable.t,
           self : 'b } -> 'a

      type ('a, 'b) table =
         int * int * int * ('a, 'b) action vector * (int -> int -> int) * (int -> int)

      val lex : 'b -> ('a, 'b) table -> symbol Streamable.t -> 'a

   end

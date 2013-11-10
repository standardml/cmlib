
signature PARSING =
   sig

      type token
      structure Streamable : MONO_STREAMABLE where type elem = token

      type 'a parser = Streamable.t -> 'a * Streamable.t

      include MONAD where type 'a m = 'a parser

      exception SyntaxError

      val accept : token parser
      val fail : 'a parser
      val lazy : (unit -> 'a parser) -> 'a parser

      val test : (token -> bool) -> token parser
      val test_ : (token -> bool) -> unit parser
      val require : ('a -> bool) -> 'a parser -> 'a parser
      val require_ : ('a -> bool) -> 'a parser -> unit parser
      val wrap : ('a -> 'b) -> 'a parser -> 'b parser
      val first : 'a parser -> 'b parser -> 'a parser
      val replace : 'a -> 'b parser -> 'a parser
      val andthen : 'a parser -> 'b parser -> ('a * 'b) parser
      val or : 'a parser -> 'a parser -> 'a parser
      val alt : 'a parser list -> 'a parser
      val andthencons : 'a parser -> 'a list parser -> 'a list parser
      val many : 'a parser -> 'a list parser
      val manyplus : 'a parser -> 'a list parser
      val option : 'a parser -> 'a option parser
      val count : int -> 'a parser -> 'a list parser

      val andthenl : 'a parser list -> 'a list parser
      val andthen2 : ('a parser * 'b parser) -> ('a * 'b) parser
      val andthen3 : ('a parser * 'b parser * 'c parser) -> ('a * 'b * 'c) parser
      val andthen4 : ('a parser * 'b parser * 'c parser * 'd parser) -> ('a * 'b * 'c * 'd) parser
      val andthen5 : ('a parser * 'b parser * 'c parser * 'd parser * 'e parser) -> ('a * 'b * 'c * 'd * 'e) parser
      val andthen6 : ('a parser * 'b parser * 'c parser * 'd parser * 'e parser * 'f parser) -> ('a * 'b * 'c * 'd * 'e * 'f) parser

   end

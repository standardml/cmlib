
signature REGEXP =
   sig

      structure Streamable : MONO_STREAMABLE where type elem = char

      datatype capture =
         One of string
       | Alt of int * capture list
       | Many of capture list list

      type parser

      val capture : parser -> parser
      val nocapture : parser -> parser

      val epsilon : parser
      val empty : parser
      val alt : parser list -> parser
      val seq : parser list -> parser
      val string : string -> parser
      val set : (char -> bool) -> parser
      val any : parser

      (* may loop if the argument parser matches the null string *)
      val star : parser -> parser
      val plus : parser -> parser

      (* matches epsilon, if the argument parser matches a prefix of the stream *)
      val lookahead : parser -> parser

      val alt' : parser list -> parser  (* nocapture o alt *)
      val star' : parser -> parser  (* nocapture o star *)
      val plus' : parser -> parser  (* nocapture o plus *)

      val match : parser -> Streamable.t -> capture list option
      val prefix : parser -> Streamable.t -> (capture list * Streamable.t) option

   end

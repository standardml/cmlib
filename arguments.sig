
signature ARGUMENTS =
   sig

      include PARSING
              where type token = string
              where type Streamable.t = string list

      exception Usage
      exception Exit

      val is : string -> unit parser                    (* matches and removes the string, raises SyntaxError if no match *)

      val int : int parser                              (* raises Usage if not an int *)
      val string : string parser                        (* same as accept *)
      val eol : unit parser                             (* raises Usage if the stream is nonempty *)

      val exec : (unit -> unit) -> unit parser          (* exec f = wrap f (return ()) *)
      val call : (string -> unit) -> unit parser        (* call f = wrap f accept *)

      val assign : 'a ref -> 'a parser -> unit parser   (* assign r p = wrap (fn x => r := x) p *)
      val set : bool ref -> unit parser                 (* set r = exec (fn () => r := true) *)
      val clear : bool ref -> unit parser               (* clear r = exec (fn () => r := false) *)


      (* full str p

         * If the first argument in the stream is str,
           removes it and invokes p.
         * If not, raises SyntaxError.
      *)
      val full : string -> unit parser -> unit parser


      (* prefix str p

         * If the first argument in the stream begins with str,
           replaces it with the suffix after str and invokes p.
         * If not, raises SyntaxError.
      *)
      val prefix : string -> unit parser -> unit parser


      (* prefix' str p

         * If the first argument in the stream begins with str, invokes p.
         * If not, raises SyntaxError.
      *)
      val prefix' : string -> unit parser -> unit parser


      (* flex str p

         * If the first argument in the stream begins with str,
           replaces it with the suffix after str and invokes p,
         * unless that suffix is empty, in which case flex removes it
           and invokes p.
         * If the first argument in the stream does not begin with str,
           raises SyntaxError.
      *)
      val flex : string -> unit parser -> unit parser


      (* scan l

         * Otherwise, scans the stream, trying each parser in l in order.
         * If one of them succeeds, then start over with the resulting stream.
         * If none of them succeeds, leave the first argument on the stream
           and continue with the tail.
         * If the tail is empty, do nothing.
      *)
      val scan : unit parser list -> unit parser


      (* scanStrict l

         * Scans the stream, trying each parser in l in order.
         * If one of them succeeds, then start over with the resulting stream.
         * If none of them succeeds, do nothing.
      *)
      val scanStrict : unit parser list -> unit parser


      (* parse p usage args

         Runs p on args.
         If p returns or raises Exit, then parse returns.
         If p raises Usage or SyntaxError, then parse prints usage then returns.
      *)
      val parse : unit parser -> string -> string list -> unit

   end


(* Parsing combinators tailored for processing command line arguments.

   The idea is the top-level parser returns a log -> log, to which you feed blank.
   Then you can read arguments out of the log.  You can also have side-effects in the
   functions you map over parsers.
 *)

signature PARSE_COMMAND_LINE =
   sig

      type 'a parser

      exception Error of string

      (* Note: all combinators commit when they return. There is no backtracking into
         earlier combinators to look for other matches.
      *)

      val return    : 'a -> 'a parser
      val string    : string parser

      val reject    : 'a parser
      val error     : string parser -> 'a parser

      val seq       : 'a parser -> 'b parser -> ('a * 'b) parser
      val or        : 'a parser list -> 'a parser
      val map       : ('a -> 'b) -> 'a parser -> 'b parser

      (* fold f x p = use p as many times as possible, folding the results *)
      val fold      : ('a * 'b -> 'b) -> 'b -> 'a parser -> 'b parser

      (* test f p = if f <next-elem> then p else backtrack *)
      val test      : (string -> bool) -> 'a parser -> 'a parser

      (* eof matches only at the end of the input *)
      val eof       : unit parser

      (* backtracking from within cut skips the enclosing or/fold *)
      val cut       : 'a parser -> 'a parser

      val parse     : 'a parser -> string list -> 'a

      val backtrack : unit -> 'a
 


      type log
      type ('a, 'b) key

      val blank     : log
      val write     : ('a, 'b) key -> 'a -> log -> log
      val read      : ('a, 'b) key -> log -> 'b

      datatype ('a, 'b) machine =
         Machine of { write : 'a -> ('a, 'b) machine,
                      read  : log -> 'b }

      val key       : ('a, 'b) machine -> ('a, 'b) key

      val mapKeyIn  : ('a -> 'b) -> ('b, 'c) key -> ('a, 'c) key
      val mapKeyOut : ('b -> 'c) -> ('a, 'b) key -> ('a, 'c) key



      (* Conversions (backtrack if they fail) *)

      val stringToBool   : string -> bool
      val stringToInt    : string -> int
      val stringToNonneg : string -> int



      (* Standard error messages *)

      (* #2 expects #1 *)
      val expects : string -> string -> string

      (* unexpected argument #1 *)
      val unexpected : string -> string

      (* unrecognized option #1 *)
      val unrecognized : string -> string



      (* Derived forms *)

      (* eq x y = x = y *)
      val eq        : string -> string -> bool

      (* forget p = map (fn _ => ()) p *)
      val forget    : 'a parser -> unit parser

      (* match f = test f (forget string) *)
      val match     : (string -> bool) -> unit parser

      val seq3      : 'a parser -> 'b parser -> 'c parser -> ('a * 'b * 'c) parser
      val seq4      : 'a parser -> 'b parser -> 'c parser -> 'd parser -> ('a * 'b * 'c * 'd) parser

      (* seqfst p q = map fst (seq p q) *)
      val seqfst    : 'a parser -> 'b parser -> 'a parser

      (* seqsnd p q = map snd (seq p q) *)
      val seqsnd    : 'a parser -> 'b parser -> 'b parser

      (* seqf = seqsnd *)
      val seqf      : 'a parser -> 'b parser -> 'b parser

      (* seql ps = List.foldl (fn (p, q) => map (op ::) (seq p q)) (return []) ps *)
      val seql      : 'a parser list -> 'a list parser

      (* trap err p = or [p, error (return err)] *)
      val trap      : string -> 'a parser -> 'a parser
      
      (* mapCompose f p = map (fun g => f o g) p *)
      val mapCompose : ('b -> 'c) -> ('a -> 'b) parser -> ('a -> 'c) parser

      (* foldCompose p = fold (op o) id p *)
      val foldCompose : ('a -> 'a) parser -> ('a -> 'a) parser

      (* mapw k p = map (write k) p *)
      val mapw      : ('a, 'b) key -> 'a parser -> (log -> log) parser

      (* ty = map stringToTy string *)
      val bool      : bool parser
      val int       : int parser
      val nonneg    : int parser

      (* nohyph = or [test (String.isPrefix "-") (error (map unrecognized string)), string] *)
      val nohyph    : string parser

      (* exactly str = match (eq str) *)
      val exactly   : string -> unit parser

      (* unitOpt str k v = map (fn () => write k v) (exactly str) *)
      val unitOpt   : string -> ('a, 'b) key -> 'a -> (log -> log) parser

      (* stringOpt str k = seqf (exactly str) (trap (expects <what> str) (map (write k) nohyph)) *)
      val stringOpt : string -> (string, 'a) key -> (log -> log) parser

      (* tyOpt str k = seqf (exactly str) (trap (expects <what> str) (map (write k) ty)) *)
      val boolOpt   : string -> (bool, 'a) key -> (log -> log) parser
      val intOpt    : string -> (int, 'a) key -> (log -> log) parser
      val nonnegOpt : string -> (int, 'a) key -> (log -> log) parser



      (* Common machines *)

      val default   : 'a -> ('a, 'a) machine
      val default'  : (log -> 'a) -> ('a, 'a) machine
      val option    : ('a, 'a option) machine
      val list      : ('a, 'a list) machine
      val listDefault    : 'a list -> ('a, 'a list) machine
      val listDefault'   : (log -> 'a list) -> ('a, 'a list) machine
      val append         : ('a list, 'a list) machine
      val appendDefault  : 'a list -> ('a list, 'a list) machine
      val appendDefault' : (log -> 'a list) -> ('a list, 'a list) machine

      val mapIn     : ('a -> 'b) -> ('b, 'c) machine -> ('a, 'c) machine
      val mapOut    : ('b -> 'c) -> ('a, 'b) machine -> ('a, 'c) machine

      type 'a id_key = ('a, 'a) key
      type 'a option_key = ('a, 'a option) key
      type 'a list_key = ('a, 'a list) key

      (* limit i err m raises (Error err) after the ith write *)
      val limit     : int -> string -> ('a, 'b) machine -> ('a, 'b) machine

      (* once = limit 1 *)
      val once      : string -> ('a, 'b) machine -> ('a, 'b) machine

   end

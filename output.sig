
signature OUTPUT =
   sig

      (* The first type argument is the type being output, the second is the monadic return type. *)
      type ('a, 'b) outputm
      type 'a output = ('a, unit) outputm

      val nothing : 'a output
      val output : 'a -> 'a output

      val return : 'b -> ('a, 'b) outputm
      val seq : ('a, 'b) outputm -> ('a, 'c) outputm -> ('a, 'c) outputm
      val lazy : (unit -> ('a, 'b) outputm) -> ('a, 'b) outputm
      val bind : ('a, 'b) outputm -> ('b -> ('a, 'c) outputm) -> ('a, 'c) outputm

      (* Uses the first function for the first output, the second for the rest. *)
      val exec : ('a, 'b) outputm -> ('a * 'c -> 'c) -> ('a * 'c -> 'c) -> 'c -> 'b * 'c

      val fold : ('a * 'b -> 'b) -> 'b -> 'a output -> 'b
      val append : 'a output -> 'a list
      val app : ('a -> unit) -> 'a output -> unit

      (* Calls the first function between adjacent outputs. *)
      val appWith : (unit -> unit) -> ('a -> unit) -> 'a output -> unit
   end

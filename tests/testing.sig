signature TESTING =
  sig
    (* expect a predicate to hold of a value; string should describe the test *)
    val expect : 'a -> ('a -> bool) -> string -> unit
    (* expect a value to be equal to a given value; string should describe the
       test *)
    val expectEq : ''a -> ''a -> string -> unit

    val passes : unit -> int
    val failures : unit -> int
    val report : unit -> unit   (* report pass/failure counts *)
    val reset : unit -> unit    (* reset pass/failure counts *)
  end


signature FINALLY =
   sig
      val finally : (unit -> 'a) -> (unit -> unit) -> 'a
   end
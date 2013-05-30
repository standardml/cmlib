
(* Secure random number generation. *)

signature RANDOM =
   sig

      val random : int -> Bytestring.string  (* generates the indicated number of bytes *)

      type seed
      val reseed : seed -> unit      

   end


(* Pollable suspensions. *)

signature PSUSP =
   sig
      
      type 'a psusp

      val delay : (unit -> 'a option) -> 'a psusp

      exception NotReady
      val force : 'a psusp -> 'a
      val isReady : 'a psusp -> bool
      val poll : 'a psusp -> 'a option

   end

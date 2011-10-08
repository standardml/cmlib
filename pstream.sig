
(* Pollable streams. *)

signature PSTREAM =
   sig

      type 'a pstream
      datatype 'a front = Nil | Cons of 'a * 'a pstream

      val eager : 'a front -> 'a pstream
      val lazy : (unit -> 'a front option) -> 'a pstream

      exception NotReady
      val front : 'a pstream -> 'a front
      val isReady : 'a pstream -> bool
      val poll : 'a pstream -> 'a front option

      datatype 'a result =
         Ready of 'a
       | Blocked
       | Closed

      val fromBlockingProcess : (unit -> 'a result) -> 'a pstream
      val fromIqueue : 'a IQueue.iqueue -> 'a pstream

      val toStream : 'a pstream -> 'a option Stream.stream

      val fix : ('a pstream -> 'a pstream) -> 'a pstream

      val map : ('a -> 'b) -> 'a pstream -> 'b pstream

   end

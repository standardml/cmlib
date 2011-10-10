
signature PRETTY_PRINT =
   sig

      type ppstream

      datatype box =
         Horizontal   (* all breaks horizontal *)
       | Vertical     (* all breaks vertical *)
       | Consistent   (* all breaks horizontal or all breaks vertical (prefer horizontal) *)
       | Freestyle    (* unrestricted (prefer horizontal breaks where possible) *)

      exception NotInBox

      val makeStream : TextIO.outstream -> int -> ppstream
      val print      : ppstream -> string -> unit
      val tie        : ppstream -> int -> unit   (* non-breakable spaces *)
      val break      : ppstream -> int -> unit
      val newline    : ppstream -> unit
      val openBox    : ppstream -> box -> int -> unit
      val closeBox   : ppstream -> unit
      val flush      : ppstream -> unit

   end

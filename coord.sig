(* A coordinate is a point location within a text document. 
 * Coordinates use a text-editor convention of counting from 1, not 0. *)

(* Adapted from Chris Okasaki, Robert Harper, and Tom Murphy VII's position
 * library. *)

signature COORD =
   sig
      type t
      
      (* Positions are positions within a file; a filename must be given *)
      val init : string -> t

      (* Advance to the next position over a regular character or a newline *)
      val nextchar : t -> t
      val nextline : t -> t

      (* Filename originally passed to init *)
      val file : t -> string

      (* Absolute position, which could be input to "M-x goto-char" in emacs *)
      val abs : t -> int 

      (* Line and character coordinates *)
      val line : t -> int
      val char : t -> int

      (* Comparision and equality are only defined on coordinates that come 
       * from the same call to "init" (morally placing them within the same 
       * document). *)
      val leftmost : t -> t -> t
      val rightmost : t -> t -> t
      val eq : (t * t) -> bool
      val compare : (t * t) -> order
      val hash : t -> word
      val toString : t -> string
   end


(* A coordinate is a point location within a text document. 
 * Coordinates use a text-editor convention of counting from 1, not 0. *)

(* Adapted from Chris Okasaki, Robert Harper, and Tom Murphy VII's position
 * library. *)

signature COORD =
   sig
      type coord
      type t = coord
      
      (* Positions are positions within a file; a filename must be given *)
      val init : string -> coord

      (* Advance to the next position over a regular character or a newline *)
      val nextchar : coord -> coord
      val nextline : coord -> coord

      (* Advance past n regular characters. *)
      val addchar : int -> coord -> coord

      (* Filename originally passed to init *)
      val file : coord -> string

      (* Absolute position, which could be input to "M-x goto-char" in emacs *)
      val abs : coord -> int 

      (* Line and character coordinates *)
      val line : coord -> int
      val char : coord -> int

      (* Comparision and equality are only defined on coordinates that come 
       * from the same call to "init" (morally placing them within the same 
       * document). *)
      val leftmost : coord -> coord -> coord
      val rightmost : coord -> coord -> coord
      val eq : (coord * coord) -> bool
      val compare : (coord * coord) -> order
      val hash : coord -> word
      val toString : coord -> string
   end


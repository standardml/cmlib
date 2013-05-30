(* A position is a selection or region of a text document; the endpoints are
 * coordinate positions. *)

(* Adapted from Chris Okasaki, Robert Harper, and Tom Murphy VII's position
 * library, as well as Kaustuv Chaudhuri's Mark library for the L2/C0
 * compiler. *)

signature POS = 
   sig
      type pos
      type t = pos

      (* Primary introduction and elimination forms *)
      val pos : Coord.coord -> Coord.coord -> pos
      val left : pos -> Coord.coord
      val right : pos -> Coord.coord

      (* Viewing the ranges as (start,finish) coordinates... *)
      (*  union (s, f) (s', f') = (min s s', max f f') *)
      (*  max   (s, f) (s', f') = (max s s', max f f') *)
      (*  min   (s, f) (s', f') = (min s s', min f f') *)
      val union  : pos -> pos -> pos
      val max    : pos -> pos -> pos
      val min    : pos -> pos -> pos

      val toString : pos -> string 
   end

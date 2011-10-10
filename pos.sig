(* A position is a selection or region of a text document; the endpoints are
 * coordinate positions. *)

(* Adapted from Chris Okasaki, Robert Harper, and Tom Murphy VII's position
 * library, as well as Kaustuv Chaudhur's Mark library for the L2/C0
 * compiler. *)

signature POS = 
   sig
      type t

      (* Primary introduction and elimination forms *)
      val pos : Coord.t -> Coord.t -> t
      val left : t -> Coord.t
      val right : t -> Coord.t

      (* Viewing the ranges as (start,finish) coordinates... *)
      (*  union (s, f) (s', f') = (min s s', max f f') *)
      (*  max   (s, f) (s', f') = (max s s', max f f') *)
      (*  min   (s, f) (s', f') = (min s s', min f f') *)
      val union  : t -> t -> t
      val max    : t -> t -> t
      val min    : t -> t -> t

      val toString : t -> string 
   end

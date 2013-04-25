
(* Not the best name for this, but we'll leave DICT for the other one for backwards compatibility. *)
signature PRE_DICT =
   sig

      type key
      type 'a dict

      exception Absent

      val empty : 'a dict
      val singleton : key -> 'a -> 'a dict
      val insert : 'a dict -> key -> 'a -> 'a dict
      val insert' : 'a dict -> key -> 'a -> 'a dict * bool    (* true if already present *)
      val remove : 'a dict -> key -> 'a dict
      val remove' : 'a dict -> key -> 'a dict * bool          (* true if present *)
      val find : 'a dict -> key -> 'a option
      val lookup : 'a dict -> key -> 'a

      (* (operate dict key absentf presentf) looks up key in dict.  If key maps to x, it replaces x with
         (presentf x).  If key is absent, it inserts absentf ().  It then returns SOME x (NONE if key
         was absent), the new value, and the new dictionary.
      *)
      val operate : 'a dict -> key -> (unit -> 'a) -> ('a -> 'a) -> 'a option * 'a * 'a dict

      (* (operate' dict key absentf presentf) looks up key in dict.  If key maps to x, it evaluates
         y = presentf(x).  If key is absent, it evaluates y = absentf ().  If y = NONE, it deletes
         the entry; if y = SOME z, it inserts or updates the entry with z.  It then returns SOME x
         (NONE if key was absent), y, and the new dictionary.
      *)
      val operate' : 'a dict -> key -> (unit -> 'a option) -> ('a -> 'a option) -> 'a option * 'a option * 'a dict

      (* (operate dict key y presentf) looks up key in dict.  If key maps to x, it replaces x with
         (presentf x).  If key is absent, it insert y.  It then returns the new dictionary.
      *)
      val insertMerge : 'a dict -> key -> 'a -> ('a -> 'a) -> 'a dict

      val isEmpty : 'a dict -> bool
      val member : 'a dict -> key -> bool

      val toList : 'a dict -> (key * 'a) list
      val domain : 'a dict -> key list
      val map : ('a -> 'b) -> 'a dict -> 'b dict
      val foldl : (key * 'a * 'b -> 'b) -> 'b -> 'a dict -> 'b
      val foldr : (key * 'a * 'b -> 'b) -> 'b -> 'a dict -> 'b
      val app : (key * 'a -> unit) -> 'a dict -> unit

   end


signature DICT =
   sig
      include PRE_DICT
      val size : 'a dict -> int
      val union : 'a dict -> 'a dict -> (key * 'a * 'a -> 'a) -> 'a dict
   end


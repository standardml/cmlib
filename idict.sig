
(* I'd rather call this TABLE, but the name is already in use. *)

signature IDICT =
   sig

      type init
      type key
      type 'a table

      exception Absent

      val table : init -> 'a table
      val reset : 'a table -> init -> unit

      val size : 'a table -> int
      val member : 'a table -> key -> bool
      val insert : 'a table -> key -> 'a -> unit
      val remove : 'a table -> key -> unit
      val find : 'a table -> key -> 'a option
      val lookup : 'a table -> key -> 'a

      (* (operate table key absentf presentf) looks up key in table.  If key maps to x, it replaces x with
         (presentf x).  If key is absent, it inserts absentf ().  It then returns SOME x (NONE if key
         was absent) and the new value.
      *)
      val operate : 'a table -> key -> (unit -> 'a) -> ('a -> 'a) -> 'a option * 'a

      (* (operate' dict key absentf presentf) looks up key in dict.  If key maps to x, it evaluates
         y = presentf(x).  If key is absent, it evaluates y = absentf ().  If y = NONE, it deletes
         the entry; if y = SOME z, it inserts or updates the entry with z.  It then returns SOME x
         (NONE if key was absent), and y.
      *)
      val operate' : 'a table -> key -> (unit -> 'a option) -> ('a -> 'a option) -> 'a option * 'a option

      val insertMerge : 'a table -> key -> 'a -> ('a -> 'a) -> unit
      val lookupOrInsert : 'a table -> key -> (unit -> 'a) -> 'a
      val lookupOrInsert' : 'a table -> key -> (unit -> 'a) -> 'a * bool   (* true if already present *)

      val toList : 'a table -> (key * 'a) list
      val fold : (key * 'a * 'b -> 'b) -> 'b -> 'a table -> 'b
      val app : (key * 'a -> unit) -> 'a table -> unit

   end

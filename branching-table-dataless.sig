
signature DATALESS_BRANCHING_TABLE =
   sig

      type init
      type key
      type table

      exception Locked
      exception Expired

      val table : init -> table
      val null : table

      (* Produces a copy that can be modified independently. *)
      val branch : table -> table

      val insert : table -> key -> unit
      val remove : table -> key -> unit
      val find : table -> key -> key option

      val parent : table -> table option

      (* Folds over the difference between the table and its parent, using
         the first function for insertions and the second for deletions.
         Takes the parent to be the empty table if there is no parent.
      *)
      val foldDiff : (key * 'b -> 'b) -> (key * 'b -> 'b) -> 'b -> table -> 'b

      val expired : table -> bool

      val compact : table -> unit  (* NB: Expires all parents. *)
      val size : table -> int      (* NB: Expires all parents. *)

   end

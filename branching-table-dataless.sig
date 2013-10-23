
signature DATALESS_BRANCHING_TABLE =
   sig

      type init
      type key
      type table

      exception Locked
      exception Expired

      val table : init -> table

      (* Produces a copy that can be modified independently. *)
      val branch : table -> table

      val size : table -> int
      val insert : table -> key -> unit
      val find : table -> key -> key option

      val parent : table -> table option

      (* Folds over the difference between the table and its parent.
         Takes the parent to be the empty table if there is no parent.
      *)
      val foldDiff : (key * 'b -> 'b) -> 'b -> table -> 'b

   end


functor DatalessBranchingTable (structure Table : MINI_DATALESS_IDICT
                                val history : int
                                val nurseryInit : Table.init)
   :>
   DATALESS_BRANCHING_TABLE
   where type init = Table.init
   where type key = Table.key
   =
   struct

      structure T = Table

      type init = T.init
      type key = T.key


      val () =
         if history < 1 then
            raise (Fail "history must be at least 1")
         else
            ()


      type stamp = int
      val nextStamp = ref 0
      fun newStamp () =
         let
            val x = !nextStamp
         in
            nextStamp := x + 1;
            x
         end


      exception Expired
      exception Locked

      (* Each base table is associated with a stamp ref.  When we update the base table,
         we also update the reference with a new stamp.  By comparing the stamp in the
         reference with a remembered stamp, we can tell if the base table has been
         updated by someone else, thereby expiring it for us.

         Each cell contains a bool ref indicating it is locked, which means that no
         more elements may be inserted into it.

         The length of the table is at most history CONS cells.
      *)

      datatype pretable =
         BASE of T.table * bool ref * stamp ref * stamp
       | CONS of T.table * bool ref * table

      withtype table = pretable ref


      fun table init =
         let
            val stamp = newStamp ()
         in
            ref (BASE (T.table init, ref false, ref stamp, stamp))
         end


      (* precondition: table has at last n CONS cells *)
      fun drop n table =
         if n = 0 then
            SOME table
         else
            (case !table of
                BASE _ =>
                   NONE
              | CONS (_, _, rest) =>
                   drop (n-1) table)


      fun branch table =
         let in
            (* Lock the table. *)
            (case !table of
                BASE (_, locked, _, _) => locked := true
              | CONS (_, locked, _) => locked := true);

            (* If we have history+1 tables in the list (counting BASE), merge the last two. *)
            (case drop (history-1) table of
                SOME (table' as ref (CONS (t, _, ref (BASE (t', locked, stampr, stamp))))) =>
                   if !stampr = stamp then
                      let
                         val stamp' = newStamp ()
                      in
                         stampr := stamp';
                         T.fold (fn (key, ()) => T.insert t' key) () t;
                         table' := BASE (t', locked, stampr, stamp')
                      end
                   else
                      raise Expired
              | SOME (ref (CONS (_, _, ref (CONS _)))) =>
                   (* More than history+1 tables in the list. *)
                   raise (Fail "invariant")
              | _ =>
                   (* Don't have a full history yet, so nothing to merge. *)
                   ());

            (* Now cons a new table onto the front. *)
            ref (CONS (T.table nurseryInit, ref false, table))
         end


      fun size table =
         let
            fun loop table acc =
               (case !table of
                   BASE (t, _, stampr, stamp) =>
                      if !stampr = stamp then
                         acc + T.size t
                      else
                         raise Expired
                 | CONS (t, _, rest) =>
                      loop rest (acc + T.size t))
         in
            loop table 0
         end


      fun insert table key =
         (case !table of
             BASE (t, locked, stampr, stamp) =>
                if !stampr = stamp then
                   if !locked then
                      raise Locked
                   else
                      T.insert t key
                else
                   raise Expired
           | CONS (t, locked, _) =>
                if !locked then
                   raise Locked
                else
                   T.insert t key)


      fun find table key =
         let
            fun loop table =
               (case !table of
                   BASE (t, _, stampr, stamp) =>
                      if !stampr = stamp then
                         T.find t key
                      else
                         raise Expired
                 | CONS (t, _, rest) =>
                      (case T.find t key of
                          ans as SOME _ =>
                             ans
                        | NONE =>
                             loop rest))
         in
            loop table
         end
                          

      fun parent table =
         (case !table of
             BASE _ =>
                NONE
           | CONS (_, _, table') =>
                SOME table')


      fun foldDiff f x table =
         (case !table of
             BASE (t, _, stampr, stamp) =>
                if !stampr = stamp then
                   T.fold f x t
                else
                   raise Expired
           | CONS (t, _, _) =>
                T.fold f x t)

   end

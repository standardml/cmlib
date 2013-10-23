
functor DatalessBranchingTable (structure Base : MINI_DATALESS_IDICT
                                structure Nursery : MINI_IDICT
                                sharing type Base.key = Nursery.key
                                val history : int
                                val nurseryInit : Nursery.init)
   :>
   DATALESS_BRANCHING_TABLE
   where type init = Base.init
   where type key = Base.key
   =
   struct

      structure B = Base
      structure N = Nursery

      type init = B.init
      type key = B.key


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


      datatype entry =
         Insert of key
       | Delete


      (* Each base table is associated with a stamp ref.  When we update the base table,
         we also update the reference with a new stamp.  By comparing the stamp in the
         reference with a remembered stamp, we can tell if the base table has been
         updated by someone else, thereby expiring it for us.

         Each cell contains a bool ref indicating it is locked, which means that no
         more modifications may be performed on it.

         The length of the table is at most history CONS cells.
      *)

      datatype pretable =
         BASE of B.table * bool ref * stamp ref * stamp
       | CONS of entry N.table * bool ref * table

      withtype table = pretable ref


      fun table init =
         let
            val stamp = newStamp ()
         in
            ref (BASE (B.table init, ref false, ref stamp, stamp))
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
                   drop (n-1) rest)


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

                         N.fold
                            (fn (_, Insert key, ()) => B.insert t' key
                              | (key, Delete, ()) => B.remove t' key)
                            () t ;

                         table' := BASE (t', locked, stampr, stamp')
                      end
                   else
                      raise Expired
              | NONE =>
                   (* Don't have a full history yet, so nothing to merge. *)
                   ()
              | SOME (ref (BASE _)) =>
                   (* Don't have a full history yet, so nothing to merge. *)
                   ()
              | SOME (ref (CONS (_, _, ref (CONS _)))) =>
                   (* More than history+1 tables in the list. *)
                   raise (Fail "invariant"));


            (* Now cons a new table onto the front. *)
            ref (CONS (N.table nurseryInit, ref false, table))
         end


      fun size table =
         let
            fun loop table acc =
               (case !table of
                   BASE (t, _, stampr, stamp) =>
                      if !stampr = stamp then
                         acc + B.size t
                      else
                         raise Expired
                 | CONS (t, _, rest) =>
                      loop rest (acc + N.size t))
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
                      B.insert t key
                else
                   raise Expired
           | CONS (t, locked, _) =>
                if !locked then
                   raise Locked
                else
                   N.insert t key (Insert key))


      fun find table key =
         let
            fun loop table =
               (case !table of
                   BASE (t, _, stampr, stamp) =>
                      if !stampr = stamp then
                         B.find t key
                      else
                         raise Expired
                 | CONS (t, _, rest) =>
                      (case N.find t key of
                          SOME (Insert key') =>
                             SOME key'
                        | SOME Remove =>
                             NONE
                        | NONE =>
                             loop rest))
         in
            loop table
         end


      fun remove table key =
         (case !table of
             BASE (t, locked, stampr, stamp) =>
                if !stampr = stamp then
                   if !locked then
                      raise Locked
                   else
                      B.remove t key
                else
                   raise Expired
           | CONS (t, locked, rest) =>
                if !locked then
                   raise Locked
                else
                   (case find rest key of
                       NONE =>
                          (* Not in the tail, so just delete from the head.
                             This will have no effect if it's not in the head either.
                          *)
                          N.remove t key
                     | SOME _ =>
                          (* Present in the tail, so mark as Deleted in the head.
                             If it's present in the head, it will be overwritten.
                          *)
                          N.insert t key Delete))


      fun parent table =
         (case !table of
             BASE _ =>
                NONE
           | CONS (_, _, table') =>
                SOME table')


      fun foldDiff fins fdel x table =
         (case !table of
             BASE (t, _, stampr, stamp) =>
                if !stampr = stamp then
                   B.fold fins x t
                else
                   raise Expired
           | CONS (t, _, _) =>
                N.fold 
                   (fn (_, Insert key, acc) => fins (key, acc)
                     | (key, Remove, acc) => fdel (key, acc))
                   x t)

   end

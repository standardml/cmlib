
structure IDeque :> IDEQUE =
   struct

      datatype 'a qbody =
         Null                (* not used in active deques *)
       | End of 'a qptr      (* front or back bookend *)
       | Node of 'a * 'a qptr * 'a qptr
      withtype 'a qptr = 'a qbody ref

      (* Invariant:
         A ideque has the form (f, b) where f is a cell before the first, and b is a cell after the last.
      *)

      type 'a ideque = 'a qbody * 'a qbody

      type idequeNode = (unit -> unit) * (unit -> bool)

      exception Orphan
      
      fun next qb =
         (case qb of
             End ptr => ptr
           | Node (_, _, ptr) => ptr
           | Null => raise Orphan)

      fun prev qb =
         (case qb of
             End ptr => ptr
           | Node (_, ptr, _) => ptr
           | Null => raise Orphan)

      fun ideque () =
         let
            val fptr = ref Null
            val f = End fptr
            val b = End (ref f)
         in
            fptr := b;
            (f, b)
         end
         
      fun isEmpty (f, b) =
         (case !(next f) of
             End _ => true
           | _ => false)

      fun reset (f, b) =
         (
         if isEmpty (f, b) then
            ()
         else
            (* disconnect from the previous elements so that a delete cannot reconnect them *)
            (
            prev (! (next f)) := Null;
            next (! (prev b)) := Null
            );
         next f := b;
         prev b := f
         )
         
      fun insertFront (f, b) x =
         let
            val curr = ! (next f)
            val new = Node (x, ref f, ref curr)
         in
            next f := new;
            prev curr := new
         end

      fun insertBack (f, b) x =
         let
            val curr = ! (prev b)
            val new = Node (x, ref curr, ref b)
         in
            prev b := new;
            next curr := new
         end

      exception Empty

      fun front (f, b) =
         (case !(next f) of
             End _ => raise Empty
           | Null => raise (Fail "invariant")
           | Node (x, _, _) => x)

      fun back (f, b) =
         (case !(prev b) of
             End _ => raise Empty
           | Null => raise (Fail "invariant")
           | Node (x, _, _) => x)

      fun removeFront (f, b) =
         (case !(next f) of
             End _ => raise Empty
           | Null => raise (Fail "invariant")
           | Node (x, bwd, fwd) =>
                (
                next f := !fwd;
                prev (!fwd) := f;
                bwd := Null;
                fwd := Null;
                x
                ))

      fun removeBack (f, b) =
         (case !(prev b) of
             End _ => raise Empty
           | Null => raise (Fail "invariant")
           | Node (x, bwd, fwd) =>
                (
                prev b := !bwd;
                next (!bwd) := b;
                bwd := Null;
                fwd := Null;
                x
                ))


      fun doDelete bwd fwd () =
         (
         next (!bwd) := !fwd;
         prev (!fwd) := !bwd;
         bwd := Null;
         fwd := Null
         )

      fun doOrphan bwd fwd () =
         (case !bwd of
             Null => true
           | _ => (case !fwd of
                      Null => true
                    | _ => false))

      fun insertFrontNode (f, b) x =
         let
            val curr = ! (next f)
            val bwd = ref f
            val fwd = ref curr
            val new = Node (x, bwd, fwd)
         in
            next f := new;
            prev curr := new;
            (doDelete bwd fwd, doOrphan bwd fwd)
         end

      fun insertBackNode (f, b) x =
         let
            val curr = ! (prev b)
            val bwd = ref curr
            val fwd = ref b
            val new = Node (x, bwd, fwd)
         in
            prev b := new;
            next curr := new;
            (doDelete bwd fwd, doOrphan bwd fwd)
         end

      fun delete (f, _) = f ()

      fun orphan (_, f) = f ()

      val dummy = ((fn () => raise Orphan), (fn () => true))

   end

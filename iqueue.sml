
structure IQueue :> IQUEUE =
   struct

      datatype 'a qbody =
         Null
       | Node of 'a * 'a qptr
      withtype 'a qptr = 'a qbody ref

      type 'a iqueue = 'a qptr ref * 'a qptr ref

      (* Invariant:
         An iqueue has the form (head, tail) where !head is a pointer to the first cell
         and !tail is a pointer just past the last cell.  Thus, !tail is always ref Null.
      *)

      fun iqueue () =
          let val ptr = ref Null
          in
             (ref ptr, ref ptr)
          end

      fun reset (head, tail) =
          let val ptr = ref Null
          in
             head := ptr;
             tail := ptr
          end

      fun isEmpty (head, tail) =
          (case !(!head) of
              Null => true
            | Node _ => false)

      fun insert (head, tail) x =
          let val lastptr = !tail
              val lastptr' = ref Null
          in
             lastptr := Node (x, lastptr');
             tail := lastptr'
          end

      exception Empty
      
      fun front (head, tail) =
          (case !(!head) of
              Null => 
                 raise Empty
            | Node (x, ptr) => 
                 x)

      fun remove (head, tail) =
          (case !(!head) of
              Null => 
                 raise Empty
            | Node (x, ptr) =>
                 (
                 head := ptr;
                 x
                 ))

   end

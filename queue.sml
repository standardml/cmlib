
structure Queue :> QUEUE =
   struct

      type 'a queue = 'a list * 'a list
      
      (* Invariant:
         A queue has the form (left, right) which is interpreted as left @ rev right.
      *)
      
      exception Empty

      val empty = ([], [])

      fun insert (l, r) x = (l, x :: r)

      fun isEmpty lr =
          (case lr of
              ([], []) => 
                 true
            | _ => 
                 false)

      fun front lr =
          (case lr of
              ([], []) => 
                 raise Empty
            | (x :: l, r) =>
                 (x, (l, r))
            | ([], r) =>
                 (case rev r of
                     x :: rest =>
                        (x, (rest, []))
                   | [] =>
                        raise (Fail "can't be nil")))

   end

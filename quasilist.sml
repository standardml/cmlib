
structure Quasilist =
   struct

      datatype 'a quasilist = 
         Zero
       | One of 'a
       | Append of 'a quasilist * 'a quasilist

      fun front ls =
          (case ls of
              nil =>
                 NONE
            | Zero :: rest =>
                 front rest
            | One x :: rest =>
                 SOME (x, rest)
            | Append (l1, l2) :: rest =>
                 front (l1 :: l2 :: rest))

      fun foldlMain f base ls =
          (case ls of
              nil =>
                 base
            | Zero :: rest =>
                 foldlMain f base rest
            | One elem :: rest =>
                 foldlMain f (f (elem, base)) rest
            | Append (l1, l2) :: rest =>
                 foldlMain f base (l1 :: l2 :: rest))

      fun foldl f x l = foldlMain f x [l]

      exception ToVector

      (* I wish this weren't imperative, but Vector doesn't provide a good functional way. *)
      fun toVector n l =
          let
             val stack = ref [l]

             fun getElem _ =
                 (case front (!stack) of
                     NONE =>
                        raise ToVector
                   | SOME (x, ls) =>
                        (
                        stack := ls;
                        x
                        ))
          in
             Vector.tabulate (n, getElem)
          end

      exception ToArray

      (* I wish this weren't imperative, but Vector doesn't provide a good functional way. *)
      fun toArray n l =
          let
             val stack = ref [l]

             fun getElem _ =
                 (case front (!stack) of
                     NONE =>
                        raise ToArray
                   | SOME (x, ls) =>
                        (
                        stack := ls;
                        x
                        ))
          in
             Array.tabulate (n, getElem)
          end

   end

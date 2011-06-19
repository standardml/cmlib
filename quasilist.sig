
signature QUASILIST =
   sig

      datatype 'a quasilist =
         Zero
       | One of 'a
       | Append of 'a quasilist * 'a quasilist

      val front : 'a quasilist -> ('a * 'a quasilist) option
      val foldl : ('a * 'b -> 'b) -> 'b -> 'a quasilist -> 'b

      exception ToVector
      val toVector : int -> 'a quasilist -> 'a vector
      (* toVector n l raises ToVector if n > |l| *)

      exception ToArray
      val toArray : int -> 'a quasilist -> 'a array
      (* toArray n l raises ToArray if n > |l| *)
         
   end

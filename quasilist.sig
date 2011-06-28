
signature QUASILIST =
   sig

      datatype 'a quasilist =
         Zero
       | One of 'a
       | Append of 'a quasilist * 'a quasilist

      val front : 'a quasilist -> ('a * 'a quasilist) option
      val foldl : ('a * 'b -> 'b) -> 'b -> 'a quasilist -> 'b

      val toList : 'a quasilist -> 'a list

   end

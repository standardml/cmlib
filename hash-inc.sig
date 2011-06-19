
signature HASH_INCREMENT =
   sig

      val hashInc : Word.word -> Word.word -> Word.word
      (* First argument is incoming hash, second is new datum. *)

   end
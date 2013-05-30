
signature CONT =
   sig

      type 'a cont
      val callcc : ('a cont -> 'a) -> 'a
      val throw : 'a cont -> 'a -> 'b

   end

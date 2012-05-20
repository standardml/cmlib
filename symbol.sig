
signature SYMBOL =
   sig

      type value
      type symbol

      val eq : symbol * symbol -> bool
      val compare : symbol * symbol -> order

      val fromValue : value -> symbol
      val toValue : symbol -> value

      val hash : symbol -> word

   end

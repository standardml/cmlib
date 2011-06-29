
signature SYMBOL_TABLE =
   sig

      type symbol

      val eq : symbol * symbol -> bool
      val compare : symbol * symbol -> order

      val fromString : string -> symbol
      val toString : symbol -> string

      val hash : symbol -> word

   end


signature PICKLE =
   sig

      type 'a pu

      val unit      : unit pu
      val bool      : bool pu
      val int       : int pu
      val intInf    : IntInf.int pu
      val char      : char pu
      val string    : string pu

      val pair      : 'a pu -> 'b pu -> ('a * 'b) pu
      val tuple2    : 'a pu -> 'b pu -> ('a * 'b) pu
      val tuple3    : 'a pu -> 'b pu -> 'c pu -> ('a * 'b * 'c) pu
      val tuple4    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> ('a * 'b * 'c * 'd) pu
      val tuple5    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> ('a * 'b * 'c * 'd * 'e) pu
      val tuple6    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> 'f pu -> ('a * 'b * 'c * 'd * 'e * 'f) pu
      val tuple7    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> 'f pu -> 'g pu -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) pu
      val tuple8    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> 'f pu -> 'g pu -> 'h pu -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) pu
      val tuple9    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> 'f pu -> 'g pu -> 'h pu -> 'i pu -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) pu

      val list      : 'a pu -> 'a list pu
      val option    : 'a pu -> 'a option pu
      val sum       : 'a pu -> 'b pu -> ('a, 'b) Sum.sum pu

      val wrap      : ('b -> 'a) -> ('a -> 'b) -> 'a pu -> 'b pu
      val fix       : ('a pu -> 'a pu) -> 'a pu
      val const     : 'a -> 'a pu
      val susp      : (unit -> 'a) -> 'a pu
      val alt       : ('a -> int) -> 'a pu list -> 'a pu
      val listish   : (unit -> 'b)                       (* nil *)
                        -> ('a * 'b -> 'b)               (* cons *)
                        -> (('a -> unit) -> 'b -> unit)  (* revapp *)
                        -> 'a pu -> 'b pu

      val share     : 'a pu -> 'a pu

      exception Error
      val pickle    : (Word8.word -> unit) -> 'a pu -> 'a -> unit
      val unpickle  : (unit -> Word8.word) -> 'a pu -> 'a
      val reset     : 'a pu -> unit

   end

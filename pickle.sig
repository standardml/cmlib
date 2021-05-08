
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
      val triple    : 'a pu -> 'b pu -> 'c pu -> ('a * 'b * 'c) pu
      val quad      : 'a pu -> 'b pu -> 'c pu -> 'd pu -> ('a * 'b * 'c * 'd) pu
      val list      : 'a pu -> 'a list pu
      val option    : 'a pu -> 'a option pu
      val sum       : 'a pu -> 'b pu -> ('a, 'b) Sum.sum pu

      val wrap      : ('b -> 'a) -> ('a -> 'b) -> 'a pu -> 'b pu
      val fix       : ('a pu -> 'a pu) -> 'a pu
      val const     : 'a -> 'a pu
      val alt       : ('a -> int) -> 'a pu list -> 'a pu
      val listish   : (unit -> 'b)                       (* nil *)
                        -> ('a * 'b -> 'b)               (* cons *)
                        -> (('a -> unit) -> 'b -> unit)  (* revapp *)
                        -> 'a pu -> 'b pu

      val share     : 'a pu -> 'a pu

      exception Error
      val pickle    : 'a pu -> (Word8.word -> unit) -> 'a -> unit
      val unpickle  : 'a pu -> (unit -> Word8.word) -> 'a
      val reset     : 'a pu -> unit

   end

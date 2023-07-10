
signature PICKLE =
   sig

      type 'a pu

      val unit      : unit pu
      val bool      : bool pu
      val int       : int pu
      val intInf    : IntInf.int pu
      val char      : char pu
      val string    : string pu
      val word8     : Word8.word pu
      val word32    : Word32.word pu
      
      val pair      : 'a pu -> 'b pu -> ('a * 'b) pu
      val tuple2    : 'a pu -> 'b pu -> ('a * 'b) pu
      val tuple3    : 'a pu -> 'b pu -> 'c pu -> ('a * 'b * 'c) pu
      val tuple4    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> ('a * 'b * 'c * 'd) pu
      val tuple5    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> ('a * 'b * 'c * 'd * 'e) pu
      val tuple6    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> 'f pu -> ('a * 'b * 'c * 'd * 'e * 'f) pu
      val tuple7    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> 'f pu -> 'g pu -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) pu
      val tuple8    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> 'f pu -> 'g pu -> 'h pu -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) pu
      val tuple9    : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> 'f pu -> 'g pu -> 'h pu -> 'i pu -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) pu
      val tuple10   : 'a pu -> 'b pu -> 'c pu -> 'd pu -> 'e pu -> 'f pu -> 'g pu -> 'h pu -> 'i pu -> 'j pu -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) pu

      (* Implementation note: a list is pickled as a series of options. *)
      val list      : 'a pu -> 'a list pu

      val option    : 'a pu -> 'a option pu

      (* Does not preserve identity of arrays as mutable data structures. *)
      val array     : 'a pu -> 'a Array.array pu

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

      val altgen    : ('a -> 'b)                         (* compute classifier *)
                      -> 'b pu                           (* classifier pickler *)
                      -> ('b -> 'a pu)                   (* data pickler *)
                      -> (('a pu -> unit) -> unit)       (* apply to all picklers *)
                      -> 'a pu

      val share     : 'a pu -> 'a pu

      (* Can skip data pickled by protect using skipProtect.
         The behavior of a protect within a share is undefined.
      *)
      val protect   : 'a pu -> 'a pu
      val skipProtect : unit pu

      val lift      : (unit -> 'a pu) -> 'a pu

      exception Error
      val pickle    : (Word8.word -> unit) -> 'a pu -> 'a -> unit
      val unpickle  : (unit -> Word8.word) -> 'a pu -> 'a
      val reset     : 'a pu -> unit

   end


signature MONAD_UTIL =
   sig

      include MONAD

      val join : 'a m m -> 'a m
      val compose : ('a -> 'b m) -> ('b -> 'c m) -> 'a -> 'c m
      val wrap : ('a -> 'b) -> 'a m -> 'b m
      val ap : ('a -> 'b) m -> 'a m -> 'b m
      val ignoreM : 'a m -> unit m

      structure MonadList :
         sig
            val sequence : 'a m list -> 'a list m
            val seql : 'a m list -> unit m
            val mapM : ('a -> 'b m) -> 'a list -> 'b list m
            val appM : ('a -> unit m) -> 'a list -> unit m
            val foldlM : ('a * 'b -> 'b m) -> 'b -> 'a list -> 'b m
            val foldrM : ('a * 'b -> 'b m) -> 'b -> 'a list -> 'b m
         end

      structure MonadArray :
         sig
            val appM : ('a -> unit m) -> 'a array -> unit m
            val appiM : (int * 'a -> unit m) -> 'a array -> unit m
            val foldliM : (int * 'a * 'b -> 'b m) -> 'b -> 'a array -> 'b m
            val foldriM : (int * 'a * 'b -> 'b m) -> 'b -> 'a array -> 'b m
            val foldlM : ('a * 'b -> 'b m) -> 'b -> 'a array -> 'b m
            val foldrM : ('a * 'b -> 'b m) -> 'b -> 'a array -> 'b m
         end

   end


signature COROUTINE =
   sig
      type ('a, 'b) corout

      val coroutine : ('a * ('b, 'a) corout -> ('a, 'b) corout) -> ('a, 'b) corout
      val yield : ('a, 'b) corout -> 'a -> 'b * ('a, 'b) corout
   end


signature SUM =
(* A sum datatype *)
   sig

      datatype ('a, 'b) sum = INL of 'a | INR of 'b

      (* Raised by the unsafe destructors, outL and outR
         if the wrong constructor is encountered *)
      exception Sum

      (* Swap the INL and INR constructors of the datatype *)
      val swap : ('a, 'b) sum -> ('b, 'a) sum

      (* Check which constructor is used to build the datatype *)
      val isL : ('a, 'b) sum -> bool
      val isR : ('a, 'b) sum -> bool

      (* Getters with a default value *)
      val getL : ('a, 'b) sum -> 'a -> 'a
      val getR : ('a, 'b) sum -> 'b -> 'b

      (* Getters without a default value *)
      val out  : ('a, 'a) sum -> 'a
      (* Might raise Sum *)
      val outL : ('a, 'b) sum -> 'a
      (* Might raise Sum *)
      val outR : ('a, 'b) sum -> 'b

      (* fold of the datatype w/ a few convenience variants *)
      val sum  : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) sum -> 'c
      val sumL : ('a -> 'b) -> ('a, 'b) sum -> 'b
      val sumR : ('b -> 'a) -> ('a, 'b) sum -> 'a

      (* map of the datatype w/ a few convenience variants *)
      val map  : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) sum -> ('c, 'd) sum
      val mapL : ('a -> 'c) -> ('a, 'b) sum -> ('c, 'b) sum
      val mapR : ('b -> 'c) -> ('a, 'b) sum -> ('a, 'c) sum
      val mapA : ('a -> 'b) -> ('a, 'a) sum -> ('b, 'b) sum

      (* monadic bind for ('a, _) sum and (_, 'b) sum monads *)
      val bindL : ('a, 'b) sum -> ('a -> ('c, 'b) sum) -> ('c, 'b) sum
      val bindR : ('a, 'b) sum -> ('b -> ('a, 'c) sum) -> ('a, 'c) sum

      (* equality *)
      val eq : ('a * 'a -> bool) -> ('b * 'b -> bool) -> (('a, 'b) sum * ('a, 'b) sum) -> bool
      (* lexicographic ordering (INL a < INR b) *)
      val collate : ('a * 'a -> order) -> ('b * 'b -> order) -> (('a, 'b) sum * ('a, 'b) sum) -> order

   end

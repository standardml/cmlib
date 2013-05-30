
structure Sum  :> SUM =
   struct
      datatype ('a, 'b) sum = INL of 'a | INR of 'b

      exception Sum

      (* Swap the INL and INR constructors of the datatype *)
      val swap = fn INL x => INR x
                  | INR x => INL x

      (* Check which constructor is used to build the datatype *)
      val isL = fn INL _ => true
                 | INR _ => false
      fun isR s = Bool.not (isL s)

      (* Getters with a default value *)
      fun getL s d = (case s of INL x => x
                              | INR _ => d)
      fun getR s d = (case s of INR x => x
                              | INL _ => d)

      (* Getters without a default value *)
      val out  = fn INL x => x
                  | INR x => x
      (* Might raise Sum *)
      val outL = fn INL x => x
                  | INR _ => raise Sum
      (* Might raise Sum *)
      val outR = fn INR x => x
                  | INL _ => raise Sum

      (* fold of the datatype w/ a few convenience variants *)
      fun sum  l r = fn INL x => l x
                      | INR x => r x
      fun sumL l   = sum l (fn x => x)
      fun sumR r   = sum (fn x => x) r

      (* map of the datatype w/ a few convenience variants *)
      fun map  l r = sum (INL o l) (INR o r)
      fun mapL l   = map l (fn x => x)
      fun mapR r   = map (fn x => x) r
      fun mapA f   = map f f

      (* monadic bind for ('a, _) sum and (_, 'b) sum monads *)
      fun bindL s f = case s of
                         INL l => f l
                       | INR r => INR r
      fun bindR s f = case s of
                         INL l => INL l
                       | INR r => f r

      (* equality *)
      fun eq eqA eqB = fn (INL a1, INL a2) => eqA (a1, a2)
                        | (INR b1, INR b2) => eqB (b1, b2)
                        | _                => false
      (* lexicographic ordering (INL a < INR b) *)
      fun collate clA clB = fn (INL a1, INL a2) => clA (a1, a2)
                             | (INL _ , INR _ ) => LESS
                             | (INR b1, INR b2) => clB (b1, b2)
                             | (INR _ , INL _ ) => GREATER
end

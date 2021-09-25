
functor RegexpFun (structure Streamable : MONO_STREAMABLE where type elem = char)
   :> REGEXP where type Streamable.t = Streamable.t
   =
   struct

      structure Streamable = Streamable
      structure S = Streamable

      datatype capture =
         One of string
       | Alt of int * capture list
       | Many of capture list list

      type caps = capture list
      type answer = caps * S.t

      type parser =
         S.t -> int -> caps -> (S.t * int * caps -> answer) -> answer

      exception Backtrack

      (* precondition: s has at least i elems *)
      fun take s i acc =
         if i = 0 then
            String.implode (rev acc)
         else
            (case S.front s of
                S.Nil => raise (Fail "precondition")

              | S.Cons (ch, s') =>
                   take s' (i-1) (ch :: acc))
         
      fun capture p s i c k =
         p s i c
         (fn (s', j, c) =>
             k (s', j, One (take s (j-i) []) :: c))

      fun nocapture p s i c k =
         p s i c
         (fn (s', j, _) => k (s', j, c))

      fun epsilon s i c k = k (s, i, c)

      fun empty _ _ _ _ = raise Backtrack

      fun alt l s i c k =
         let
            fun loop l n =
               (case l of
                   [] => raise Backtrack

                 | p :: rest =>
                      (p s i []
                          (fn (s', j, c') =>
                              k (s', j, Alt (n, c') :: c))
                       handle Backtrack => loop rest (n+1)))
         in
            loop l 0
         end

      fun seq l s i c k =
         let
            fun loop l (sic as (s, i, c)) =
               (case l of
                   [] => k sic

                 | p :: rest =>
                      p s i c (loop rest))
         in
            loop l (s, i, c)
         end

      fun string str s i c k =
         let
            val len = String.size str

            fun loop s i j =
               if j = len then
                  k (s, i, c)
               else 
                  (case S.front s of
                      S.Nil => raise Backtrack

                    | S.Cons (ch, s') =>
                         if ch = String.sub (str, j) then
                            loop s' (i+1) (j+1)
                         else
                            raise Backtrack)
         in
            loop s i 0
         end

      fun set f s i c k =
         (case S.front s of
             S.Nil => raise Backtrack

           | S.Cons (ch, s') =>
                if f ch then
                   k (s', i+1, c)
                else
                   raise Backtrack)

      fun any s i c k =
         (case S.front s of
             S.Nil => raise Backtrack

           | S.Cons (_, s') =>
                k (s', i+1, c))

      fun starMain p s i acc k =
         k (s, i, acc)
         handle Backtrack =>
            p s i []
            (fn (s', j, c) =>
                starMain p s' j (c :: acc) k)

      fun star p s i c k =
         starMain p s i []
         (fn (s', j, acc) =>
             k (s', j, Many acc :: c))

      fun starMain' p s i k =
         k (s, i)
         handle Backtrack =>
            p s i []
            (fn (s', j, _) =>
                starMain' p s' j k)

      fun star' p s i c k =
         starMain' p s i
         (fn (s', j) => k (s', j, c))

      fun plus p = seq [p, star p]


      fun lookahead p s i c k =
         p s i c
         (fn _ => k (s, i, c))


      fun alt' ps = nocapture (alt ps)
      fun plus' p = nocapture (seq [p, star' p])

      fun match p s =
         let
            val (c, _) =
               p s 0 []
               (fn (s', _, c) =>
                   (case S.front s' of
                       S.Nil => (c, s')
                       
                     | S.Cons _ => raise Backtrack))
         in
            SOME c
         end
         handle Backtrack => NONE

      fun prefix p s =
         SOME (p s 0 []
                  (fn (s', _, c) => (c, s')))
         handle Backtrack => NONE

   end

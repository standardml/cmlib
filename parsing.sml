
functor ParsingFun (type token
                    structure Streamable : MONO_STREAMABLE where type elem = token)
   : PARSING
   =
   struct
   
      type token = token

      structure Streamable = Streamable

      type 'a parser = Streamable.t -> 'a * Streamable.t
      type 'a m = 'a parser

      fun return x s = (x, s)

      fun bind p1 p2 s =
         let
            val (x1, s1) = p1 s
         in
            p2 x1 s1
         end

      fun seq p1 p2 s =
         let
            val (_, s1) = p1 s
         in
            p2 s1
         end



      exception SyntaxError

      fun accept s =
         (case Streamable.front s of
             Streamable.Cons (x, s') =>
                (x, s')
           | Streamable.Nil =>
                raise SyntaxError)

      fun fail s =
         raise SyntaxError

      fun lazy f s = f () s



      fun require f p =
         bind p (fn x => if f x then return x else raise SyntaxError)

      fun require_ f p =
         bind p (fn x => if f x then return () else raise SyntaxError)

      fun test f = require f accept

      fun test_ f = require_ f accept

      fun wrap f p =
         bind p (fn x => return (f x))

      fun first p1 p2 s =
         let
            val (x, s1) = p1 s
            val (_, s2) = p2 s1
         in
            (x, s2)
         end

      fun replace token p = seq p (return token)

      fun andthen p1 p2 =
         bind p1 (fn x1 => bind p2 (fn x2 => return (x1, x2)))

      fun or p1 p2 s =
         (p1 s
          handle SyntaxError => p2 s)

      fun alt pl =
         List.foldr (fn (p1, p2) => or p1 p2) fail pl

      fun andthencons p1 p2 = wrap (op ::) (andthen p1 p2)

      fun many p s =
         or (andthencons p (many p)) (return nil) s

      fun manyplus p =
         andthencons p (many p)

      fun option p = or (wrap SOME p) (return NONE)

      fun count n p s =
         if n = 0 then
            return [] s
         else
            (wrap (op ::)
                (andthen p (count (n-1) p))) s

      fun andthenl pl =
         List.foldr (fn (p1, p2) => wrap (op ::) (andthen p1 p2)) (return []) pl

      fun andthen2 (p1, p2) =
         bind p1 (fn x1 => bind p2 (fn x2 => return (x1, x2)))

      fun andthen3 (p1, p2, p3) =
         bind p1 (fn x1 => bind p2 (fn x2 => bind p3 (fn x3 => return (x1, x2, x3))))

      fun andthen4 (p1, p2, p3, p4) =
         bind p1 (fn x1 => bind p2 (fn x2 => bind p3 (fn x3 => bind p4 (fn x4 => return (x1, x2, x3, x4)))))

      fun andthen5 (p1, p2, p3, p4, p5) =
         bind p1 (fn x1 => bind p2 (fn x2 => bind p3 (fn x3 => bind p4 (fn x4 => bind p5 (fn x5 => return (x1, x2, x3, x4, x5))))))

      fun andthen6 (p1, p2, p3, p4, p5, p6) =
         bind p1 (fn x1 => bind p2 (fn x2 => bind p3 (fn x3 => bind p4 (fn x4 => bind p5 (fn x5 => bind p6 (fn x6 => return (x1, x2, x3, x4, x5, x6)))))))

   end

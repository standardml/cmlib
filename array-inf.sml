
structure ArrayInf :> ARRAY_INF =
   struct

      type 'a array = 'a Array.array ref * 'a

      fun array x = (ref (Array.array (0, x)), x)

      fun sub ((ar, x), i) =
         Array.sub (!ar, i)
         handle Subscript =>
            if i < 0 then
               raise Subscript
            else
               x

      fun update ((ar, x), i, y) =
         Array.update (!ar, i, y)
         handle Subscript =>
            if i < 0 then
               raise Subscript
            else
               let
                  val n = Int.max (i+1, Array.length (!ar) * 2)
                  val a = Array.array (n, x)
               in
                  Array.copy {di=0, src= !ar, dst=a};
                  ar := a;
                  Array.update (a, i, y)
               end

      fun erase (ar, x) =
         ar := Array.array (0, x)

      fun isEmpty (ar, _) =
         Array.length (!ar) = 0

   end
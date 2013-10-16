
structure ArrayUtil :> ARRAY_UTIL =
   struct

      fun foldriLazy f x arr =
         let
            val len = Array.length arr
            
            fun loop i =
               if i >= len then
                  x
               else
                  f (i, Array.sub (arr, i), Susp.delay (fn () => loop (i+1)))
         in
            loop 0
         end

      fun foldrLazy f x arr =
         let
            val len = Array.length arr
            
            fun loop i =
               if i >= len then
                  x
               else
                  f (Array.sub (arr, i), Susp.delay (fn () => loop (i+1)))
         in
            loop 0
         end

      fun foldliLazy f x arr =
         let
            val len = Array.length arr
            
            fun loop i =
               if i < 0 then
                  x
               else
                  f (i, Array.sub (arr, i), Susp.delay (fn () => loop (i-1)))
         in
            loop (len-1)
         end

      fun foldlLazy f x arr =
         let
            val len = Array.length arr
            
            fun loop i =
               if i < 0 then
                  x
               else
                  f (Array.sub (arr, i), Susp.delay (fn () => loop (i-1)))
         in
            loop (len-1)
         end

   end



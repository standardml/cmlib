
functor MonadUtilFun (structure Monad : MONAD)
   :>
   MONAD_UTIL where type 'a m = 'a Monad.m
   =
   struct

      structure Monad = Monad

      open Monad

      fun join m = bind m (fn m' => m')

      fun compose f g x = bind (f x) g

      fun wrap f m = bind m (fn x => return (f x))

      fun ap fm m = bind fm (fn f => bind m (fn x => return (f x)))

      fun ignoreM m = bind m (fn _ => return ())


      structure MonadList =
         struct

            fun mapM f l =
               let
                  fun loop l acc =
                     (case l of
                         [] =>
                            return (rev acc)
                       | h :: t =>
                            bind (f h)
                            (fn x => loop t (x :: acc)))
               in
                  loop l []
               end
            
            fun appM f l =
               (case l of
                   [] =>
                      return ()
                 | h :: t =>
                      bind (f h)
                      (fn () => appM f t))
      
            (* We could get sequence and seql from mapM and appM, but we'll optimize them. *)
      
            fun sequence l =
               let
                  fun loop l acc =
                     (case l of
                         [] =>
                            return (rev acc)
                       | h :: t =>
                            bind h
                            (fn x => loop t (x :: acc)))
               in
                  loop l []
               end
            
            fun seql l =
               (case l of
                   [] =>
                      return ()
                 | h :: t =>
                      bind h
                      (fn _ => seql t))
      
            fun foldlM f x l =
               (case l of
                   [] =>
                      return x
                 | h :: t =>
                      bind (f (h, x))
                      (fn y => foldlM f y t))
      
            fun foldrM f x l = foldlM f x (rev l)

         end


      structure MonadArray =
         struct

            fun appiM f arr =
               let
                  val len = Array.length arr

                  fun loop i =
                     if i >= len then
                        return ()
                     else
                        bind (f (i, Array.sub (arr, i)))
                        (fn () => loop (i+1))
               in
                  loop 0
               end

            fun appM f arr =
               let
                  val len = Array.length arr

                  fun loop i =
                     if i >= len then
                        return ()
                     else
                        bind (f (Array.sub (arr, i)))
                        (fn () => loop (i+1))
               in
                  loop 0
               end

            fun foldliM f x arr =
               let
                  val len = Array.length arr
                  
                  fun loop i acc =
                     if i >= len then
                        return acc
                     else
                        bind (f (i, Array.sub (arr, i), acc))
                        (fn acc' => loop (i+1) acc')
               in
                  loop 0 x
               end

            fun foldlM f x arr =
               let
                  val len = Array.length arr
                  
                  fun loop i acc =
                     if i >= len then
                        return acc
                     else
                        bind (f (Array.sub (arr, i), acc))
                        (fn acc' => loop (i+1) acc')
               in
                  loop 0 x
               end

            fun foldriM f x arr =
               let
                  val len = Array.length arr
                  
                  fun loop i acc =
                     if i < 0 then
                        return acc
                     else
                        bind (f (i, Array.sub (arr, i), acc))
                        (fn acc' => loop (i-1) acc')
               in
                  loop (len-1) x
               end

            fun foldrM f x arr =
               let
                  val len = Array.length arr
                  
                  fun loop i acc =
                     if i < 0 then
                        return acc
                     else
                        bind (f (Array.sub (arr, i), acc))
                        (fn acc' => loop (i-1) acc')
               in
                  loop (len-1) x
               end

         end

   end

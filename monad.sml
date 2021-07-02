
structure OptionMonad :> MONAD where type 'a m = 'a option =
   struct

      type 'a m = 'a option

      val return = SOME

      fun seq x y =
         (case x of
             NONE => NONE
           | SOME _ => y)

      fun bind x f =
         (case x of
             NONE => NONE
           | SOME x' => f x')

   end


structure ListMonad :> MONAD where type 'a m = 'a list =
   struct

      type 'a m = 'a list

      fun return x = [x]

      fun bind l f =
         foldr
         (fn (x, r) => f x @ r)
         nil
         l

      fun seq l l' = bind l (fn _ => l')
   
   end

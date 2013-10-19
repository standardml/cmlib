
functor CostringFun (structure String : MINI_STRING)
   :>
   COSTRING
   where type elem = String.elem
   where type string = String.string
   =
   struct

      structure S = String
      structure M = Stream

      type elem = S.elem
      type string = S.string
      
      type costring = S.string * S.string M.stream


      fun getc (str, stm) =
         if S.size str > 0 then
            SOME (S.sub (str, 0), (S.extract (str, 1, NONE), stm))
         else
            (case M.front stm of
                M.Nil => NONE
              | M.Cons cos => getc cos)


      structure Streamable =
         struct
            type t = costring
            type elem = elem

            datatype front = Nil | Cons of elem * costring

            fun front cos =
               (case getc cos of
                   NONE => Nil
                 | SOME x => Cons x)
         end


      fun full str = (str, M.eager M.Nil)

      val null : costring = (S.null, M.eager M.Nil)

      fun fromStream stm = (S.null, stm)


      fun minSize ((str, stm), n) =
         let
            val sz = S.size str
         in
            n <= sz
            orelse
            (case M.front stm of
                M.Nil => false
              | M.Cons cos =>
                   minSize (cos, n-sz))
         end


      fun maxSize ((str, stm), n) =
         let
            val sz = S.size str
         in
            n >= sz
            andalso
            (case M.front stm of
                M.Nil => true
              | M.Cons cos =>
                   maxSize (cos, n-sz))
         end

      fun isEmpty cos = maxSize (cos, 0)


      (* find (str, stm, n)

         if    n >= 0
         then  |str| + |stm| >= n
               str :: stm = str' :: str'' :: stm'
               n' = n - |str'|
               0 <= n' <= |str''|
               and
               return (str'', n', stm')

               orelse

               |str| + |stm| < n
               and
               raise Subscript
      *)
      fun find (str, stm, n) =
         let
            val sz = S.size str
         in
            if n <= sz then
               (str, n, stm)
            else
               (case M.front stm of
                   M.Nil =>
                      raise Subscript
                 | M.Cons (str', stm') =>
                      find (str', stm', n-sz))
         end


      fun hd stm =
         (case M.front stm of
             M.Nil =>
                raise Subscript
           | M.Cons (str, stm') =>
                if S.size str > 0 then
                   S.sub (str, 0)
                else
                   hd stm')


      (* collect (str, stm, j, acc)

         if    j > 0
         then  |str| + |stm| >= j
               str :: stm = str' :: str'' :: stm'
               |str'| = j
               concat (rev acc') = concat (rev acc) ^ str'
               and
               return (acc', str'', stm')

               orelse
               
               |str| + |stm| < j
               and
               raise Subscript
      *)
      fun collect (str, stm, j, acc) =
         let
            val sz = S.size str
         in
            if j <= sz then
               (S.extract (str, 0, SOME j) :: acc,
                S.extract (str, j, NONE),
                stm)
            else
               (case M.front stm of
                   M.Nil =>
                      raise Subscript
                 | M.Cons (str', stm') =>
                      collect (str', stm', j-sz, str :: acc))
         end


      fun sub ((str, stm), i) =
         let
            val (str', i', stm') = find (str, stm, i)
         in
            if i' < S.size str' then
               S.sub (str', i')
            else
               (* i' = S.size str', so we want the first elem in stm *)
               hd stm'
         end


      fun slice ((str, stm), i, j) =
         if i < 0 orelse j < 0 then
            raise Subscript
         else
            let
               val (str', i', stm') = find (str, stm, i)
               val sz = S.size str'
            in
               if i' + j <= sz then
                  S.extract (str', i', SOME j)
               else
                  let
                     val (acc, _, _) = collect (S.extract (str', i', NONE), stm', j, [])
                  in
                     S.concat (rev acc)
                  end
            end


      fun suffix ((str, stm), i) =
         if i < 0 then
            raise Subscript
         else
            let
               val (str', i', stm') = find (str, stm, i)
            in
               (S.extract (str', i', NONE), stm')
            end


      fun splitAt ((str, stm), i) =
         if i < 0 then
            raise Subscript
         else
            let
               val (acc, str', stm') = collect (str, stm, i, [])
            in
               (S.concat (rev acc), (str', stm'))
            end


      fun all (str, stm) =
         let
            fun loop (stm, acc) =
               (case M.front stm of
                   M.Nil =>
                      S.concat (rev acc)
                 | M.Cons (str, stm') =>
                      loop (stm', str :: acc))
         in
            loop (stm, [str])
         end


      fun fromProcess f =
         let
            fun read () =
               let
                  val str = f ()
               in
                  if S.size str = 0 then
                     NONE
                  else
                     SOME str
               end
         in
            fromStream (Stream.fromProcess read)
         end

   end



structure SubstringCostring = CostringFun (structure String = SubstringMiniString)

structure BytesubstringCostring = CostringFun (structure String = BytesubstringMiniString)

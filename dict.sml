
functor DictFun (D : PRE_DICT)
   :>
   DICT
   where type key = D.key
   =
   struct

      type key = D.key
      type 'a dict = (int * 'a D.dict)

      fun size (n, _) = n

      exception Absent = D.Absent

      val empty = (0, D.empty)

      fun singleton key datum = (1, D.singleton key datum)

      fun insert (n, d) key datum =
         let
            val (d', present) = D.insert' d key datum
            val n' = if present then n else n+1
         in
            (n', d')
         end

      fun insert' (n, d) key datum =
         let
            val (d', present) = D.insert' d key datum
            val n' = if present then n else n+1
         in
            ((n', d'), present)
         end

      fun remove (n, d) key =
         let
            val (d', present) = D.remove' d key
            val n' = if present then n-1 else n
         in
            (n', d')
         end

      fun remove' (n, d) key =
         let
            val (d', present) = D.remove' d key
            val n' = if present then n-1 else n
         in
            ((n', d'), present)
         end

      fun operate' (n, d) key absentf presentf =
         let
            val (old, new, d') = D.operate' d key absentf presentf
            val n' =
               (case (old, new) of
                   (NONE, NONE) => n
                 | (SOME _, NONE) => n-1
                 | (NONE, SOME _) => n+1
                 | (SOME _, SOME _) => n)
         in
            (old, new, (n', d'))
         end

      fun operate dict key absentf presentf =
         let
            val (x, y, d) = operate' dict key (SOME o absentf) (SOME o presentf)
         in
            (x, valOf y, d)
         end
         
      fun insertMerge dict key x f =
         #3 (operate' dict key (fn () => SOME x) (SOME o f))

      fun union (dict1 as (n1, d1)) (dict2 as (n2, d2)) f =
         if n1 <= n2 then
            D.foldl
            (fn (key, datum, dict) =>
                insertMerge dict key datum
                (fn datum' => f (key, datum, datum')))
            dict2
            d1
         else
            D.foldl
            (fn (key, datum, dict) =>
                insertMerge dict key datum
                (fn datum' => f (key, datum', datum)))
            dict1
            d2

      fun find (_, d) key = D.find d key
      fun lookup (_, d) key = D.lookup d key
      fun isEmpty (_, d) = D.isEmpty d
      fun member (_, d) key = D.member d key
      fun toList (_, d) = D.toList d
      fun domain (_, d) = D.domain d
      fun map f (n, d) = (n, D.map f d)
      fun foldl f x (_, d) = D.foldl f x d
      fun foldr f x (_, d) = D.foldr f x d
      fun app f (_, d) = D.app f d

   end

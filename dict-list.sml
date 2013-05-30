
functor ListPreDict (structure Key : ORDERED)
   :> PRE_DICT where type key = Key.t
   =
   struct

      type key = Key.t
      type 'a dict = (key * 'a) list

      exception Absent

      val empty = []

      val isEmpty = null

      fun singleton key x = [(key, x)]

      fun insert l key x =
         (case l of
             [] => [(key, x)]
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       (key, x) :: l
                  | EQUAL =>
                       (key, x) :: rest
                  | GREATER =>
                       (key', y) :: insert rest key x))

      fun insert' l key x =
         (case l of
             [] =>
                ([(key, x)], false)
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       ((key, x) :: l, false)
                  | EQUAL =>
                       ((key, x) :: rest, true)
                  | GREATER =>
                       let
                          val (d, present) = insert' rest key x
                       in
                          ((key', y) :: d, present)
                       end))

      fun remove l key =
         (case l of
             [] => []
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS => l
                  | EQUAL => rest
                  | GREATER =>
                       (key', y) :: remove rest key))

      fun remove' l key =
         (case l of
             [] => ([], false)
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS => (l, false)
                  | EQUAL => (rest, true)
                  | GREATER =>
                       let
                          val (d, present) = remove' rest key
                       in
                          ((key', y) :: d, present)
                       end))

      fun operate' l key absentf presentf =
         (case l of
             [] =>
                (case absentf () of
                    NONE =>
                       (NONE, NONE, [])
                  | z as SOME x =>
                       (NONE, z, [(key, x)]))
           | (key', y) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       (case absentf () of
                           NONE =>
                              (NONE, NONE, l)
                         | z as SOME x =>
                              (NONE, z, (key, x) :: l))
                  | EQUAL =>
                       (case presentf y of
                           NONE =>
                              (SOME y, NONE, rest)
                         | z as SOME x =>
                              (SOME y, z, (key, x) :: rest))
                  | GREATER =>
                       let
                          val (ante, post, rest') = operate' rest key absentf presentf
                       in
                          (ante, post, (key', y) :: rest')
                       end))

      fun operate dict key absentf presentf =
         let
            val (x, y, d) = operate' dict key (SOME o absentf) (SOME o presentf)
         in
            (x, valOf y, d)
         end
         
      fun insertMerge dict key x f =
         #3 (operate' dict key (fn () => SOME x) (SOME o f))

      fun find l key =
         (case l of
             [] => 
                NONE
           | (key', x) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       NONE
                  | EQUAL =>
                       SOME x
                  | GREATER =>
                       find rest key))

      fun lookup l key =
         (case l of
             [] => 
                raise Absent
           | (key', x) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       raise Absent
                  | EQUAL =>
                       x
                  | GREATER =>
                       lookup rest key))

      fun member l key =
         (case l of
             [] =>
                false
           | (key', _) :: rest =>
                (case Key.compare (key, key') of
                    LESS =>
                       false
                  | EQUAL =>
                       true
                  | GREATER =>
                       member rest key))

      fun toList l = l

      fun domain l = List.map (fn (key, _) => key) l

      fun map f l = List.map (fn (key, x) => (key, f x)) l

      fun foldl f base l = List.foldl (fn ((key, x), y) => f (key, x, y)) base l

      fun foldr f base l = List.foldr (fn ((key, x), y) => f (key, x, y)) base l

      val app = List.app

   end


functor ListDict (structure Key : ORDERED)
   :>
   DICT where type key = Key.t
   =
   DictFun (ListPreDict (structure Key = Key))

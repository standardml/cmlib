
functor ListDict (structure Key : ORDERED)
   :> DICT where type key = Key.t
   =
   struct

      type key = Key.t
      type 'a dict = (key * 'a) list

      exception Absent

      val empty = []

      val isEmpty = null

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

      fun insertMerge l key x f =
          (case l of
              [] => [(key, x)]
            | (key', y) :: rest =>
                 (case Key.compare (key, key') of
                     LESS =>
                        (key, x) :: l
                   | EQUAL =>
                        (key, f y) :: rest
                   | GREATER =>
                        (key', y) :: insertMerge rest key x f))

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

      fun union l1 l2 f =
          (case (l1, l2) of
              ([], _) =>
                 l2
            | (_, []) => 
                 l1
            | ((entry1 as (key1, x1)) :: rest1, (entry2 as (key2, x2)) :: rest2) =>
                 (case Key.compare (key1, key2) of
                     LESS =>
                        entry1 :: union rest1 l2 f
                   | GREATER =>
                        entry2 :: union l1 rest2 f
                   | EQUAL =>
                        (key1, f (key1, x1, x2)) :: union rest1 rest2 f))

      fun toList l = l

      fun map f l = List.map (fn (key, x) => (key, f x)) l

      fun foldl f base l = List.foldl (fn ((key, x), y) => f (key, x, y)) base l

      fun foldr f base l = List.foldr (fn ((key, x), y) => f (key, x, y)) base l

      val app = List.app

   end

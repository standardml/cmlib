
functor ListSet (structure Elem : ORDERED)
   :> SET where type elem = Elem.t
   =
   struct

      type elem = Elem.t
      type set = elem list

      val empty = []

      val isEmpty = List.null

      fun singleton elem = [elem]

      fun insert l elem =
          (case l of
              [] => [elem]
            | elem' :: rest =>
                 (case Elem.compare (elem, elem') of
                     LESS =>
                        elem :: l
                   | EQUAL =>
                        l
                   | GREATER =>
                        elem' :: insert rest elem))

      fun remove l elem =
          (case l of
              [] => []
            | elem' :: rest =>
                 (case Elem.compare (elem, elem') of
                     LESS =>
                        l
                   | EQUAL =>
                        rest
                   | GREATER =>
                        elem' :: remove rest elem))

      fun member l elem =
          (case l of
              [] => false
            | elem' :: rest =>
                 (case Elem.compare (elem, elem') of
                     LESS =>
                        false
                   | EQUAL =>
                        true
                   | GREATER =>
                        member rest elem))

      fun union l1 l2 =
          (case (l1, l2) of
              ([], _) =>
                 l2
            | (_, []) =>
                 l1
            | (elem1 :: rest1, elem2 :: rest2) =>
                 (case Elem.compare (elem1, elem2) of
                     LESS =>
                        elem1 :: union rest1 l2
                   | GREATER =>
                        elem2 :: union l1 rest2
                   | EQUAL =>
                        elem1 :: union rest1 rest2))

      fun intersection l1 l2 =
          (case (l1, l2) of
              ([], _) =>
                 []
            | (_, []) =>
                 []
            | (elem1 :: rest1, elem2 :: rest2) =>
                 (case Elem.compare (elem1, elem2) of
                     LESS =>
                        intersection rest1 l2
                   | GREATER =>
                        intersection l1 rest2
                   | EQUAL =>
                        elem1 :: intersection rest1 rest2))

      fun difference l1 l2 =
          (case (l1, l2) of
              ([], _) =>
                 []
            | (_, []) =>
                 l1
            | (elem1 :: rest1, elem2 :: rest2) =>
                 (case Elem.compare (elem1, elem2) of
                     LESS =>
                        elem1 :: difference rest1 l2
                   | EQUAL =>
                        difference rest1 rest2
                   | GREATER =>
                         difference l1 rest2))

      fun eq (l1, l2) =
         (case (l1, l2) of
             ([], []) => 
                true
           | (elem1 :: rest1, elem2 :: rest2) =>
                Elem.eq (elem1, elem2)
                andalso
                eq (rest1, rest2)
           | _ =>
                false)

      fun subset (l1, l2) =
         (case (l1, l2) of
             ([], _) => 
                true
           | (elem1 :: rest1, elem2 :: rest2) =>
                (case Elem.compare (elem1, elem2) of
                    LESS =>
                       false
                  | EQUAL =>
                       subset (rest1, rest2)
                  | GREATER =>
                       subset (l1, rest2))
           | (_ :: _, [])  =>
                false)

      val size = length

      fun toList l = l
      val foldl = List.foldl
      val foldr = List.foldl
      val app = List.app

   end


signature SET =
   sig
      type elem
      type set

      val empty : set
      val isEmpty : set -> bool
      val singleton : elem -> set
      val insert : set -> elem -> set
      val remove : set -> elem -> set
      val member : set -> elem -> bool
      val union : set -> set -> set
      val intersection : set -> set -> set
      val difference : set -> set -> set

      val eq : set * set -> bool
      val subset : set * set -> bool
      val size : set -> int

      val toList : set -> elem list
      val foldl : (elem * 'a -> 'a) -> 'a -> set -> 'a
      val foldr : (elem * 'a -> 'a) -> 'a -> set -> 'a
      val app : (elem -> unit) -> set -> unit
  end

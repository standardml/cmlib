
signature MINI_DICT =
   sig

      type key

      type 'a dict

      val empty : 'a dict
      val insert : 'a dict -> key -> 'a -> 'a dict
      val remove : 'a dict -> key -> 'a dict
      val find : 'a dict -> key -> 'a option

   end

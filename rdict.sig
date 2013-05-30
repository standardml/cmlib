
(* Dictionary with range searches. *)
signature RDICT =
   sig

      include PRE_DICT

      val partition : 'a dict -> key -> 'a dict * 'a option * 'a dict
      val partitionlt : 'a dict -> key -> 'a dict * 'a dict
      val partitiongt : 'a dict -> key -> 'a dict * 'a dict

      val rangeii : 'a dict -> key -> key -> 'a dict   (* inclusive left, inclusive right *)
      val rangeie : 'a dict -> key -> key -> 'a dict   (* inclusive left, exclusive right *)
      val rangeei : 'a dict -> key -> key -> 'a dict   (* exclusive left, inclusive right *)
      val rangeee : 'a dict -> key -> key -> 'a dict   (* exclusive left, exclusive right *)
      
      val least : 'a dict -> 'a
      val greatest : 'a dict -> 'a
      val leastGt : 'a dict -> key -> 'a
      val leastGeq : 'a dict -> key -> 'a
      val greatestLt : 'a dict -> key -> 'a
      val greatestLeq : 'a dict -> key -> 'a

   end

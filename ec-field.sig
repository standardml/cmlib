
(* Actually a family of fields. *)

signature EC_FIELD =
   sig

      type index
      type elem

      val validIndex : index -> bool
      val validElem : index * elem -> bool

      val size : index -> IntInf.int

      val eq : elem * elem -> bool
      val zero : index -> elem
      val one : index -> elem
      val plus : index * elem * elem -> elem
      val minus : index * elem * elem -> elem
      val negate : index * elem -> elem
      val times : index * elem * elem -> elem
      val inverse : index * elem -> elem

      val elemToBytes : index * elem -> Bytestring.string
      val elemFromBytes : Bytestring.string -> elem
      val elemToInt : elem -> IntInf.int
      
   end

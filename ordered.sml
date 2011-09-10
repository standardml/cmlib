
structure UnitOrdered
   :> ORDERED where type t = unit
   =
   struct
      type t = unit
      fun eq _ = true
      fun compare _ = EQUAL
   end


structure IntOrdered
   :> ORDERED where type t = int
   =
   struct
      type t = int

      val eq : int * int -> bool = op =
      val compare = Int.compare
   end


structure StringOrdered
   :> ORDERED where type t = string
   =
   struct
      type t = string

      val eq : string * string -> bool = op =
      val compare = String.compare
   end


structure CharOrdered
   :> ORDERED where type t = char
   =
   struct
      type t = char

      val eq : char * char -> bool = op =
      val compare = Char.compare
   end


functor InvertOrdered (Ordered : ORDERED)
   :> ORDERED where type t = Ordered.t
   =
   struct

      type t = Ordered.t
      val eq = Ordered.eq
      
      fun compare p =
         (case Ordered.compare p of
             LESS => GREATER
           | EQUAL => EQUAL
           | GREATER => LESS)

   end

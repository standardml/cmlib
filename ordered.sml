
structure CharOrdered
   :> ORDERED where type t = char
   =
   struct
      type t = char

      val eq : char * char -> bool = op =
      val compare = Char.compare
   end


structure IntOrdered
   :> ORDERED where type t = int
   =
   struct
      type t = int

      val eq : int * int -> bool = op =
      val compare = Int.compare
   end


structure IntInfOrdered
   :> ORDERED where type t = IntInf.int
   =
   struct
      type t = IntInf.int
      
      val eq : IntInf.int * IntInf.int -> bool = (op =)  
      val compare = IntInf.compare
   end


structure TimeOrdered
   :> ORDERED where type t = Time.time
   =
   struct
      type t = Time.time

      val eq : Time.time * Time.time -> bool = op =
      val compare = Time.compare
   end


structure StringOrdered
   :> ORDERED where type t = string
   =
   struct
      type t = string

      val eq : string * string -> bool = op =
      val compare = String.compare
   end


structure UnitOrdered
   :> ORDERED where type t = unit
   =
   struct
      type t = unit
      fun eq _ = true
      fun compare _ = EQUAL
   end


functor ListOrdered (Ordered : ORDERED)
   :> ORDERED where type t = Ordered.t list
   =
   struct
      type t = Ordered.t list
  
      fun compare ([], []) = EQUAL
        | compare (_, []) = GREATER
        | compare ([], _) = LESS
        | compare (x :: xs, y :: ys) =  
          case Ordered.compare (x, y) of
             EQUAL => compare (xs, ys)
           | ord => ord
      fun eq (xs, ys) = EQUAL = compare (xs, ys)
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

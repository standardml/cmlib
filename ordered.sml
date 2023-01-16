
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


structure WordOrdered
   :> ORDERED where type t = Word.word
   =
   struct
      type t = Word.word
      val eq : Word.word * Word.word -> bool = op =
      val compare = Word.compare
   end


functor PairOrdered (structure Ordered1 : ORDERED
                     structure Ordered2 : ORDERED)
   :> ORDERED where type t = Ordered1.t * Ordered2.t
   =
   struct

      type t = Ordered1.t * Ordered2.t

      fun eq ((x1, x2), (y1, y2)) =
         Ordered1.eq (x1, y1) andalso Ordered2.eq (x2, y2)

      fun compare ((x1, x2), (y1, y2)) =
         (case Ordered1.compare (x1, y1) of
             EQUAL => Ordered2.compare (x2, y2)
           | ord => ord)

   end


functor ListOrdered (structure Ordered : ORDERED)
   :> ORDERED where type t = Ordered.t list
   =
   struct
      type t = Ordered.t list
  
      fun compare ([], []) = EQUAL
        | compare (_, []) = GREATER
        | compare ([], _) = LESS
        | compare (x :: xs, y :: ys) =  
             (case Ordered.compare (x, y) of
                 EQUAL => compare (xs, ys)
               | ord => ord)

      fun eq (xs, ys) =
         (case compare (xs, ys) of
             EQUAL => true
           | _ => false)
   end


functor InvertOrdered (structure Ordered : ORDERED)
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

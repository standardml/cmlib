
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

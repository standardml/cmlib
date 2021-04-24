
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

      val compare = Time.compare
      fun eq (t1, t2) = (case compare (t1, t2) of EQUAL => true | _ => false)
   end




structure TimeOrdered
   :> ORDERED where type t = Time.time
   =
   struct
      type t = Time.time

      val compare = Time.compare
      fun eq (t1, t2) = (case compare (t1, t2) of EQUAL => true | _ => false)
   end



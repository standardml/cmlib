
structure Cont:> CONT =
   struct
      type 'a cont = 'a MLton.Cont.t
      val callcc = MLton.Cont.callcc
      fun throw k x = MLton.Cont.throw (k, x)  
   end

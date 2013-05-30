structure Weak:> WEAK = 
   struct
      type 'a weak = 'a MLton.Weak.t
      val weak = MLton.Weak.new
      val strong = MLton.Weak.get
   end

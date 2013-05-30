
signature ARITH =
   sig

      val sqrt : IntInf.int -> IntInf.int

      (* factor2 x = (d, s) where d * 2^s *)
      val factor2 : IntInf.int -> IntInf.int * IntInf.int

      (* if isprime x = false then x is composite
         if isprime x = true  then x is prime with high probablity
      *)
      val isprime : IntInf.int -> bool

      (* invmod (x, q) = y where x * y = 1 (mod q) *)
      val invmod : IntInf.int * IntInf.int -> IntInf.int

      (* powmod (y, x, q) = z where y^x = z (mod q) *)
      val powmod : IntInf.int * IntInf.int * IntInf.int -> IntInf.int

      (* if    p is prime
         then  sqrtmod (x, p) = r where r * r = x (mod p)
      *)
      val sqrtmod : IntInf.int * IntInf.int -> IntInf.int
      exception NotSquare

   end


structure ConvertIntInf :> CONVERT_INT_INF =
   struct

      open IntInf


      fun toLoop len acc x =
         if x = 0 then
            (len, acc)
         else
            toLoop (Int.+ (len, 1)) (Word8.fromInt (IntInf.toInt (andb (x, 0xff))) :: acc) (~>> (x, 0w8))

      fun toBytesB x =
         if x <= 0 then
            if x = 0 then
               Bytestring.str 0w0
            else
               let
                  val x' = IntInf.+ (IntInf.<< (1, Word.+ (Word.fromInt (IntInf.log2 (IntInf.~ x)), 0w1)), x)
               
                  val (_, l) = toLoop 0 [] x'
               in
                  Bytestring.implode l
               end
         else 
            let
               val (_, l) = toLoop 0 [] x
            in
               Bytestring.implode l
            end

      fun toBytesL x = Bytestring.rev (toBytesB x)

      fun dupOnto x i l =
         if Int.<= (i, 0) then
            l
         else
            dupOnto x (Int.- (i, 1)) (x :: l)
      
      fun toFixedBytesB (digits, x) =
         if Int.<= (digits, 0) then
            raise Domain
         else
            if x >= 0 then
               let
                  val (len, l) = toLoop 0 [] x
               in
                  if Int.> (len, digits) then
                     raise Overflow
                  else
                     Bytestring.implode (dupOnto 0w0 (Int.- (digits, len)) l)
               end
            else
               let
                  val x' = IntInf.+ (IntInf.<< (1, Word.+ (Word.fromInt (IntInf.log2 (IntInf.~ x)), 0w1)), x)
                  val (len, l) = toLoop 0 [] x'
               in
                  if Int.> (len, digits) then
                     raise Overflow
                  else
                     Bytestring.implode (dupOnto 0wxff (Int.- (digits, len)) l)
               end

      fun toFixedBytesL x = Bytestring.rev (toFixedBytesB x)
   

      fun fromLoop acc l =
         (case l of
             [] => acc
           | b :: t =>
                fromLoop (orb (<< (acc, 0w8), IntInf.fromInt (Word8.toInt b))) t)

      fun fromBytesB s =
         fromLoop 0 (Bytestring.explode s)

      fun fromBytesL s = fromBytesB (Bytestring.rev s)

      fun fromSignedBytesB (sign, s) =
         if sign then
            fromLoop 0 (Bytestring.explode s) - << (1, Word.fromInt (Int.* (8, Bytestring.size s)))
         else
            fromLoop 0 (Bytestring.explode s)

      fun fromSignedBytesL (sign, s) = fromSignedBytesB (sign, Bytestring.rev s)

   end

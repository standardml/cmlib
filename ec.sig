
signature ELLIPTIC_CURVE =
   sig

      structure Field : EC_FIELD

      type curve = {index : Field.index, a : Field.elem, b : Field.elem}
      type point = (Field.elem * Field.elem) option

      val validCurve : curve -> bool
      val validPoint : (curve * point) -> bool

      val infinity : point
      val plus : curve * point * point -> point
      val negate : curve * point -> point
      val double : curve * point -> point
      val eq : point * point -> bool

      val parity : curve * point -> bool
      val recoverPoint : curve * Field.elem * bool -> point option

   end

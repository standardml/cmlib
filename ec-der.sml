
functor EllipticCurveDERFun (structure EllipticCurve : ELLIPTIC_CURVE)
   :>
   ELLIPTIC_CURVE_CODING
   where type data = Bytestring.string
   where type EC.Field.index = EllipticCurve.Field.index
   where type EC.Field.elem = EllipticCurve.Field.elem
   =
   struct

      type data = Bytestring.string

      structure EC = EllipticCurve

      type param = { curve : EC.curve,
                     base : EC.point,
                     order : IntInf.int,
                     cofactor : IntInf.int }

      type privkey = IntInf.int
      type pubkey = EC.point
      type sg = IntInf.int * IntInf.int

      structure B = Bytestring
      structure D = DER


      val fmtField =
         D.tuple2 (D.objectid,                                                      (* field type *)
                   D.union (D.unsigned,                                             (* prime field size *)
                            D.tuple3 (D.unsigned,                                   (* m *)
                                      D.objectid,                                   (* binary field type *)
                                      D.union (D.null,                              (* "onBasis" ?? *)
                                               D.union (D.unsigned,                 (* trinomial k *)
                                                        D.tuple3 (D.unsigned,       (* pentanomial k1 *)
                                                                  D.unsigned,       (* pentanomial k2 *)
                                                                  D.unsigned))))))  (* pentanomial k3 *)
      
      val fmtParams =
         D.union (D.objectid,                                                       (* curve name *)
                  D.union (D.tuple6 (D.fixed (1, D.unsigned),                       (* version *)
                                     fmtField,                                      (* field *)
                                     D.tuple3 (D.bytestring,                        (* a *)
                                               D.bytestring,                        (* b *)
                                               D.option D.bitstring),               (* seed *)
                                     D.bytestring,                                  (* base *)
                                     D.unsigned,                                    (* order *)
                                     D.option (D.unsigned)),                        (* cofactor *)
                           D.null))                                                 (* "implicitlyCA" ?? *)
      
      val fmtPrivkey = 
         D.tuple4 (D.fixed (1, D.unsigned),                                         (* version *)
                   D.bytestring,                                                    (* privkey *)
                   D.option (D.tag (0w0, fmtParams)),                               (* params *)
                   D.option (D.tag (0w1, D.bitstring)))                             (* pubkey *)
      
      val fmtSg = D.tuple2 (D.unsigned, D.unsigned)



      exception Invalid


      (* The public key encodings are not actually DER encodings, but they're what are done. *)

      fun encodePubkey ({curve={index, ...}, ...}:param, pt) =
         (case pt of
             NONE =>
                (* Infinity is not a legal pubkey. *)
                raise Invalid
           | SOME (x, y) =>
                B.concat
                [B.str 0wx04, EC.Field.elemToBytes (index, x), EC.Field.elemToBytes (index, y)])


      fun encodePubkeyCompressed ({curve, ...}:param, pt) =
         (case pt of
             NONE =>
                (* Infinity is not a legal pubkey. *)
                raise Invalid
           | SOME (x, y) =>
                let
                   val header =
                      if EC.parity (curve, pt) then
                         0wx03
                      else
                         0wx02
                in
                   B.^ (B.str header, EC.Field.elemToBytes (#index curve, x))
                end)


      fun decodePubkey ({curve, ...}:param, str) =
         let
            val b = B.sub (str, 0)
         in
            if b = 0wx04 orelse b = 0wx06 orelse b = 0wx07 then
               let
                  val len = B.size str
                  val elemlen = (IntInf.log2 (EC.Field.size (#index curve)) div 8) + 1
               in
                  if len = elemlen * 2 + 1 then
                     let
                        val x = EC.Field.elemFromBytes (B.substring (str, 1, elemlen))
                        val y = EC.Field.elemFromBytes (B.substring (str, elemlen+1, elemlen))
                        val pt = SOME (x, y)
                     in
                        if EC.validPoint (curve, pt) then
                           pt
                        else
                           raise Invalid
                     end
                  else
                     raise Invalid
               end
            else if b = 0wx02 orelse b = 0wx03 then
               let
                  val len = B.size str
                  val elemlen = (IntInf.log2 (EC.Field.size (#index curve)) div 8) + 1
               in
                  if len = elemlen + 1 then
                     let
                        val x = EC.Field.elemFromBytes (B.substring (str, 1, elemlen))
                     in
                        (case EC.recoverPoint (curve, x, b = 0wx03) of
                            NONE => raise Invalid
                          | SOME pt =>
                               (* This check should be redundant, but just to be safe... *)
                               if EC.validPoint (curve, pt) then
                                  pt
                               else
                                  raise Invalid)
                     end
                  else
                     raise Invalid
               end
            else
               (* Either:
                  - the byte is zero, which represents infinity, an illegal public key, or
                  - the byte is invalid.
               *)
               raise Invalid
         end


      fun encodePrivkey key =
         D.encode fmtPrivkey ((), ConvertIntInf.toBytesB key, NONE, NONE)
         handle D.DecodeError _ => raise Invalid

      fun decodePrivkey s =
         let
            val ((), str, _, _) =
               D.decode fmtPrivkey s
               handle D.DecodeError _ => raise Invalid
         in
            ConvertIntInf.fromBytesB str
         end

      fun encodeKeypair (param, pubkey, privkey) =
         D.encode fmtPrivkey ((), ConvertIntInf.toBytesB privkey, NONE, SOME (encodePubkey (param, pubkey), 0))

      fun decodeKeypair (param, s) =
         ((case D.decode fmtPrivkey s of
              ((), priv, _, SOME (pubkey, 0)) =>
                 (decodePubkey (param, pubkey), ConvertIntInf.fromBytesB priv)
            | _ =>
                 raise Invalid)
          handle D.DecodeError _ => raise Invalid)

      fun encodeSg sg =
         D.encode fmtSg sg
         handle D.DecodeError _ => raise Invalid

      fun decodeSg s =
         D.decode fmtSg s
         handle D.DecodeError _ => raise Invalid

   end


structure ECDERp = EllipticCurveDERFun (structure EllipticCurve = EllipticCurveFp)
structure ECDER2m = EllipticCurveDERFun (structure EllipticCurve = EllipticCurveF2m)

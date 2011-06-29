
structure SymbolTable :> SYMBOL_TABLE =
   struct

      structure H =
         HashTableFun
         (structure Key = StringHashable)

      val nextIndex = ref 0

      val table : (int * string) H.table = H.table 3001

      type symbol = int * string
            
      fun eq ((n1:int, _), (n2, _)) = n1 = n2
             
      fun compare ((n1, _), (n2, _)) = Int.compare (n1, n2)
            
      fun fromString str =
         H.lookupOrInsert table str
         (fn () =>
             let val n = !nextIndex
             in
                nextIndex := !nextIndex + 1;
                (n, str)
             end)
             
      fun toString (_ , str) = str

      fun hash (n, _) = Word.fromInt n

   end


structure SymbolOrdered
   :> ORDERED where type t = SymbolTable.symbol
   =
   struct
      type t = SymbolTable.symbol

      val eq = SymbolTable.eq
      val compare = SymbolTable.compare
   end

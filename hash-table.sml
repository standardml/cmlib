
(* XX Reimplement this *)

functor HashTableFun (structure Key : HASHABLE)
   :> HASH_TABLE where type key = Key.t
   =
   struct

      structure T = HashTableFn (struct
                                    type hash_key = Key.t
                                    val hashVal = Key.hash
                                    val sameKey = Key.eq
                                 end)

      type key = Key.t
      type 'a table = 'a T.hash_table

      exception Absent

      fun table n = T.mkTable (n, Absent)

      val member = T.inDomain

      fun insert table key x = T.insert table (key, x)

      val lookup = T.lookup
      val find = T.find

      fun lookupOrInsert table key f =
          (case T.find table key of
              NONE =>
                 let
                    val x = f ()
                 in
                    T.insert table (key, x);
                    x
                 end
            | SOME x =>
                 x)

      val fold = T.foldi

   end
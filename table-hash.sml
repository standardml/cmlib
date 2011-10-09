
functor HashTableTable (structure HashTable : HASH_TABLE
                        type value
                        val size : int)
   :> TABLE where type key = HashTable.key and type value = value
   =
   struct

      type key = HashTable.key
      type value = value
      type checkpoint = (key * value) list

      val table : value HashTable.table = HashTable.table size
      
      fun reset () = HashTable.reset table size

      fun save () = HashTable.toList table

      fun restore chk = 
         (HashTable.reset table size 
         ; app (fn (key, value) => HashTable.insert table key value) chk)

      fun member key = HashTable.member table key

      fun insert key value = HashTable.insert table key value

      fun remove key = HashTable.remove table key

      fun find key = HashTable.find table key

      fun lookup key = HashTable.lookup table key

      fun operate key absentf presentf = 
         HashTable.operate table key absentf presentf

      fun insertMerge key x f = 
         HashTable.insertMerge table key x f
  
      fun toList () = HashTable.toList table

      fun fold f base = HashTable.fold f base table
  
      fun app f = HashTable.app f table

   end

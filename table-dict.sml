
functor DictTable (structure Dict : DICT
                   type value)
   :> TABLE where type key = Dict.key and type value = value
   =
   struct

      type key = Dict.key
      type value = value
      type checkpoint = value Dict.dict

      val dict : value Dict.dict ref = ref Dict.empty
      
      fun reset () = dict := Dict.empty

      fun save () = (!dict)

      fun restore chk = dict := chk

      fun member key = Dict.member (!dict) key

      fun insert key value = dict := Dict.insert (!dict) key value

      fun remove key = dict := Dict.remove (!dict) key

      fun find key = Dict.find (!dict) key

      fun lookup key = Dict.lookup (!dict) key

      fun operate key absentf presentf = 
         let 
            val (old, new, newdict) = Dict.operate (!dict) key absentf presentf
         in
            (old, new) before dict := newdict
         end

      fun insertMerge key x f = 
         dict := Dict.insertMerge (!dict) key x f
  
      fun toList () = Dict.toList (!dict)

      fun fold f base = Dict.foldl f base (!dict)
  
      fun app f = Dict.app f (!dict)

   end

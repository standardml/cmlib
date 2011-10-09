
functor DictMultiTable (structure Dict : DICT
                        type value)
   :> MULTI_TABLE where type key = Dict.key and type value = value
   = 
   struct

      type key = Dict.key 
      type value = value
      
      val dict : value list Dict.dict ref = ref Dict.empty
    
      fun reset () = dict := Dict.empty

      fun member key = Dict.member (!dict) key

      fun insert key value = 
         let 
            val newdict = 
               Dict.insertMerge 
                  (!dict)
                  key 
                  [ value ] 
                  (fn vals => value :: vals)
         in
            dict := newdict
         end

      fun find key = 
         case Dict.find (!dict) key of
            NONE => []
          | SOME vals => vals
         
      val lookup = find

      fun toList () = Dict.toList (!dict)

end

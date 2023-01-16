
(* For types where we'd like to minimize the number of comparisons. *)

functor HashDict (structure Key : HASHABLE
                  structure Dict : DICT where type key = Key.t
                  structure WordDict : DICT where type key = Word.word)
   :> MINI_DICT where type key = Key.t
   =
   struct

      type key = Dict.key

      type 'a dict = 'a Dict.dict WordDict.dict

      val empty = WordDict.empty

      fun insert wd key x =
         let
            val (_, _, wd') =
               WordDict.operate wd (Key.hash key)
                  (fn () => Dict.singleton key x)
                  (fn kd => Dict.insert kd key x)
         in
            wd'
         end

      fun remove wd key =
         let
            val (_, _, wd') =
               WordDict.operate' wd (Key.hash key)
                  (fn () => NONE)
                  (fn kd =>
                      let
                         val kd' = Dict.remove kd key
                      in
                         if Dict.isEmpty kd' then
                            NONE
                         else
                            SOME kd'
                      end)
         in
            wd'
         end

      fun find wd key =
         (case WordDict.find wd (Key.hash key) of
             NONE => NONE

           | SOME kd => Dict.find kd key)

   end


structure IntPickleable :> PICKLEABLE_TYPE where type t = int =
   struct
      
      type t = int
      val pu = Pickle.int

   end


structure IntInfPickleable :> PICKLEABLE_TYPE where type t = IntInf.int =
   struct
      
      type t = IntInf.int
      val pu = Pickle.intInf

   end


structure StringPickleable :> PICKLEABLE_TYPE where type t = string =
   struct
      
      type t = string
      val pu = Pickle.string

   end


functor DictPickleable (structure Key : PICKLEABLE_TYPE
                        structure Dict : DICT where type key = Key.t)
   :> PICKLEABLE_CON where type 'a t = 'a Dict.dict
   =
   struct

      structure P = Pickle

      type 'a t = 'a Dict.dict

      fun pu puData =
         P.listish
            (fn () => Dict.empty)
            (fn ((k, x), d) => Dict.insert d k x)
            Dict.app
            (P.pair Key.pu puData)
            
   end


functor SetPickleable (structure Elem : PICKLEABLE_TYPE
                       structure Set : SET where type elem = Elem.t)
   :> PICKLEABLE_TYPE where type t = Set.set
   =
   struct

      structure P = Pickle

      type t = Set.set

      val pu =
         P.listish
            (fn () => Set.empty)
            (fn (e, s) => Set.insert s e)
            Set.app
            Elem.pu

   end


functor SymbolPickleable (structure Value : PICKLEABLE_TYPE
                          structure Symbol : SYMBOL where type value = Value.t)
   :> PICKLEABLE_TYPE where type t = Symbol.symbol
   =
   struct

      type t = Symbol.symbol

      val pu =
         Pickle.wrap
            Symbol.toValue
            Symbol.fromValue
            Value.pu

   end

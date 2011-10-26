functor SetDictTestFun (Dict: DICT where type key = int):> sig end =
struct
   val e1: string Dict.dict = Dict.empty
   val () = Testing.expect e1 Dict.isEmpty "Emptiness"

   val e2: string Dict.dict = Dict.empty
   val e3 = Dict.insert (Dict.insert e2 9 "bar") 3 "foo"
   val () = Testing.expect e2 Dict.isEmpty "Persistant emptiness"
   val () = Testing.expect e3 (not o Dict.isEmpty) "Non-emptiness"
   val () = Testing.expect e3 (fn d => Dict.member d 3) "Membership 1"
   val () = Testing.expect e3 (fn d => Dict.member d 9) "Membership 2"
   val () = Testing.expect e3 (fn d => not (Dict.member d 10)) "Membership 3"

   val e4 = Dict.remove e3 3 
   val e5 = Dict.remove e4 9
   val () = Testing.expect e4 (not o Dict.isEmpty) "Removal 1"
   val () = Testing.expect e5 Dict.isEmpty "Removal 2"
   val () = Testing.expectEq (Dict.size e2) 0 "Size 1"
   val () = Testing.expectEq (Dict.size e3) 2 "Size 2"
   val () = Testing.expectEq (Dict.size e4) 1 "Size 3"
   val () = Testing.expectEq (Dict.size e5) 0 "Size 4"

   val e5 = SOME (Dict.remove e1 1) handle _ => NONE
   val () = Testing.expect e5 isSome "Removal of a non-existant object"

   val e6 = SOME (Dict.remove e4 3) handle _ => NONE
   val () = Testing.expect e6 isSome "Double removal"

   val i7 = Dict.find e3 9
   val i8 = Dict.find e3 10
   val i9 = SOME (Dict.lookup e3 9) handle Dict.Absent => NONE
   val iA = SOME (Dict.lookup e3 10) handle Dict.Absent => NONE
   val () = Testing.expectEq i7 (SOME "bar") "Finding a present object"
   val () = Testing.expectEq i8 NONE "Finding an absent object"
   val () = Testing.expectEq i9 (SOME "bar") "Lookup on a present object"
   val () = Testing.expectEq iA NONE "Lookup on an absent object"

   val () = Testing.expectEq (Dict.toList e3) [ (3, "foo"), (9, "bar") ]
               "Dict.toList"
   val () = Testing.expectEq (Dict.domain e3) [ 3, 9 ]
               "Dict.Domain"
   val () = Testing.expectEq (Dict.foldl (fn (_, a, b) => a ^ b) "" e3)
               "barfoo" "Dict.foldl"
   val () = Testing.expectEq (Dict.foldr (fn (_, a, b) => a ^ b) "" e3)
               "foobar" "Dict.foldr"

   (* The following are just to check the types of interface functions *)
   (* They should be turned into checked tests *)
   val _: string Dict.dict = Dict.union e3 e3 (fn (_, x, y) => x ^ y)
   val _: string option * string * string Dict.dict =
      Dict.operate e3 9 (fn () => "baz") (fn s => "X" ^ s)
   val _: string option * string * string Dict.dict =
      Dict.operate e3 10 (fn () => "baz") (fn s => "X" ^ s)
   val _: string Dict.dict =
      Dict.insertMerge e3 9 "baz" (fn s => "baz" ^ s)
   val _: string Dict.dict =
      Dict.insertMerge e3 10 "baz" (fn s => "baz" ^ s)
   val _: int Dict.dict = Dict.map size e3
   val () = Dict.app (fn (x: int * string) => ()) e3
end

structure SetDictTest = 
struct
   val () = print "Dict test (sets-dicts-test.sml)\n" 
   val () = Testing.reset ()
   structure S = SetDictTestFun (IntSplayDict)
   structure S = SetDictTestFun (IntRedBlackDict)
   structure S = SetDictTestFun (IntListDict)
   val () = Testing.report ()
end

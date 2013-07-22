

signature COLLECTION =
   sig
      type t
      eqtype u
      
      val new : unit -> t
      val insert : t -> int * int -> unit
      val remove : t -> int -> unit
      val expose : t -> u list
   end

      
functor SetToCollection (Set : SET where type elem = int)
   :>
   COLLECTION
   where type t = Set.set ref
   where type u = int
   =
   struct
      type t = Set.set ref
      type u = int

      fun new () = ref Set.empty
      fun insert r (key, datum) = r := Set.insert (!r) key
      fun remove r key = r := Set.remove (!r) key
      fun expose r = Set.toList (!r)
   end


functor DictToCollection (Dict : DICT where type key = int)
   :>
   COLLECTION
   where type t = int Dict.dict ref
   where type u = int * int
   =
   struct
      type t = int Dict.dict ref
      type u = int * int

      fun new () = ref Dict.empty

      fun insert r (key, datum) = r := Dict.insert (!r) key datum

      fun remove r key = r := Dict.remove (!r) key

      fun expose r = Dict.toList (!r)
   end


functor TestFun (structure Control : COLLECTION
                 structure Exper : COLLECTION
                 sharing type Control.u = Exper.u
                 structure Rand : RAND
                 val extra : Exper.t -> unit
                 val name : string)
   =
   struct

      val control = Control.new ()
      val exper = Exper.new ()

      type u = Control.u

      val maximum = 10000

      fun test n =
         if n <= 0 then
            let
               val c = Control.expose control
               val e = Exper.expose exper
            in
               if c = e then
                  ()
               else
                  raise (Fail (name ^ " test failed"))
            end
         else
            if Rand.randBool () then
               let
                  val key = Rand.randInt maximum
                  val datum = Rand.randInt maximum
               in
                  Control.insert control (key, datum);
                  ((Exper.insert exper (key, datum); extra exper)
                   handle exn =>
                      (
                      print "insert ";
                      print (Int.toString n);
                      print "\n";
                      raise exn
                      ));
                  test (n-1)
               end
            else
               let
                  val key = Rand.randInt maximum
               in
                  Control.remove control key;
                  ((Exper.remove exper key; extra exper)
                   handle exn =>
                      (
                      print "remove ";
                      print (Int.toString n);
                      print "\n";
                      raise exn
                      ));
                  test (n-1)
               end

      val _ = test 50000

      val () =
         print (name ^ " test passed\n")

   end


(* Need to expose the representation of RedBlackDict to use this test. *)
structure TestRedBlack =
   struct

      open RedBlackTree

      fun testRedBlackInv tree =
         (case tree of
             Leaf => ()
           | Node (RED, _, left, right) =>
                (
                testRedBlackInvRed left;
                testRedBlackInvRed right
                )
           | Node (BLACK, _, left, right) =>
                (
                testRedBlackInv left;
                testRedBlackInv right
                ))

      and testRedBlackInvRed tree =
         (case tree of
             Leaf => ()
           | Node (RED, _, left, right) =>
                raise (Fail "red-black invariant")
           | Node (BLACK, _, left, right) =>
                (
                testRedBlackInv left;
                testRedBlackInv right
                ))

      fun testBlackHeightInv tree =
         (case tree of
             Leaf => 0
           | Node (color, _, left, right) =>
                let
                   val m = testBlackHeightInv left
                   val n = testBlackHeightInv right
                in
                   if m = n then
                      (case color of
                          RED => m
                        | BLACK => m+1)
                   else
                      raise (Fail "black-height invariant")
                end)

      fun testInv (_, tree) =
         (
         testBlackHeightInv tree;
         testRedBlackInv tree
         )

   end


structure ListSetColl = 
   SetToCollection (ListSet (structure Elem = IntOrdered))

structure ListDictColl =
   DictToCollection (ListDict (structure Key = IntOrdered))

structure SplaySetColl =
   SetToCollection (SplaySet (structure Elem = IntOrdered))

structure SplayDictColl =
   DictToCollection (SplayDict (structure Key = IntOrdered))

structure RedBlackSetColl =
   SetToCollection (RedBlackSet (structure Elem = IntOrdered))

structure RedBlackDictColl =
   DictToCollection (SplayDict (structure Key = IntOrdered))


structure HashTableColl : COLLECTION =
   struct
      structure H = HashTable (structure Key = IntHashable)

      type t = int H.table
      type u = int * int

      fun new () = H.table 100
      fun insert r (key, datum) = H.insert r key datum
      fun remove r key = H.remove r key

      fun expose r =
         Mergesort.sort
         (fn ((key1, _), (key2, _)) => Int.compare (key1, key2))
         (H.toList r)
   end


val () = MTRand.reseed (Word32.fromLargeInt (Time.toMilliseconds (Time.now ())));

structure Test = TestFun (structure Control = ListSetColl
                          structure Exper = SplaySetColl
                          structure Rand = MTRand
                          fun extra _ = ()
                          val name = "SplaySet")

structure Test = TestFun (structure Control = ListDictColl
                          structure Exper = SplayDictColl
                          structure Rand = MTRand
                          fun extra _ = ()
                          val name = "SplayDict")

structure Test = TestFun (structure Control = ListSetColl
                          structure Exper = RedBlackSetColl
                          structure Rand = MTRand
                          fun extra _ = ()
                          val name = "RedBlackDict")

structure Test = TestFun (structure Control = ListDictColl
                          structure Exper = RedBlackDictColl
                          structure Rand = MTRand
                          fun extra _ = ()
                          val name = "RedBlackDict")

structure Test = TestFun (structure Control = ListDictColl
                          structure Exper = HashTableColl
                          structure Rand = MTRand
                          fun extra _ = ()
                          val name = "HashTable")

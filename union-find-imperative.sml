
structure IUnionFind :> IMPERATIVE_UNION_FIND
   =
   struct

      datatype 'a state =
         Root of 'a * int
       | Child of 'a set

      withtype 'a set = 'a state ref

      fun new label = ref (Root (label, 0))

      val eq : 'a set * 'a set -> bool = op =

      fun root set =
         (case !set of
             Root _ =>
                set

           | Child parent =>
                let
                   val ancestor = root parent
                in
                   set := Child ancestor;
                   ancestor
                end)

      fun union f set1 set2 =
         let
            (* root always returns a Root state *)
            val ancestor1 as ref (Root (label1, rank1)) = root set1
            val ancestor2 as ref (Root (label2, rank2)) = root set2
         in
            if ancestor1 = ancestor2 then
               ()
            else if rank1 < rank2 then
               (
               ancestor1 := Child ancestor2;
               ancestor2 := Root (f (label2, label1), rank2)
               )
            else if rank1 > rank2 then
               (
               ancestor2 := Child ancestor1;
               ancestor1 := Root (f (label1, label2), rank1)
               )
            else (* rank1 = rank2 *)
               (
               ancestor2 := Child ancestor1;
               ancestor1 := Root (f (label1, label2), rank1+1)
               )
         end

      fun sameSet (set1, set2) = root set1 = root set2

      fun find set =
         (case ! (root set) of
             Root (label, _) => label
           | _ => raise (Fail "invariant"))

      fun isCanonical set =
         (case ! set of
             Root _ => true
           | Child _ => false)

   end

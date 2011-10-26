structure ListSequence
   :> 
   sig
      include SEQUENCE
              where type 'a seq = 'a list
   end
   =
struct
   exception NYI (* not yet implemented *)

   type 'a seq = 'a list
 
   datatype 'a treeview =
      EMPTY
    | ELT of 'a 
    | NODE of ('a seq * 'a seq)
 
   fun showt l =
      (case l of
          [] => EMPTY
        | [x] =>
             ELT x
        | h :: t => 
             NODE ([h], t))
 
   fun hidet t =
      (case t of
          EMPTY =>
             []
        | ELT x => 
             [x]
        | NODE (l1, l2) =>
             l1 @ l2)
 
   datatype 'a listview =
      NIL
    | CONS of ('a * 'a seq)

   fun showl l =
      (case l of
          [] => NIL
        | h :: t =>
             CONS (h, t))

   fun hidel l =
      (case l of
          NIL => []
        | CONS (h, t) =>
             h :: t)

   type 'a ord = 'a * 'a -> order
 
   fun empty () = []
   
   fun singleton x = [x]
 
   val length = List.length
   fun nth l i = List.nth (l, i)
   val map = List.map
   fun fromList l = l
   val append = op @
   fun toList l = l
   val cons = op ::
   val fold = List.foldl
   val foldl = List.foldl
   val foldr = List.foldr

   fun iter f b s = foldl (fn (x, y) => f (y, x)) b s
 
   fun tabulate _ _ = raise NYI                   
   fun collate _ = raise NYI
   fun map2 _ _ _ = raise NYI
   fun reduce _ _ _ = raise NYI
   fun scan _ _ _ = raise NYI
   fun filter _ _ = raise NYI    
   fun foldlh _ _ _ = raise NYI
   fun iterh _ _ _ = raise NYI
   fun flatten _ = raise NYI
   fun partition _ _ = raise NYI
   fun inject _ _ = raise NYI
   fun take _ = raise NYI
   fun drop _ = raise NYI
   fun rake _ _ = raise NYI
   fun subseq _ _ = raise NYI
   fun splitMid _ = raise NYI
   fun sort _ _ = raise NYI
   fun merge _ _ = raise NYI
   fun collect _ _ = raise NYI
   fun toString _ _ = raise NYI
   fun tokens _ _ = raise NYI
   fun fields _ _ = raise NYI
   fun showti _ _ = raise NYI
   fun hidet _ = raise NYI

end

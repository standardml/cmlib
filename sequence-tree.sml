
(* We want to reveal that TreeSequence.seq = TreeSequence.treeview.
   We should just say "sig include SEQUENCE sharing type seq = treeview end",
   but a bug in SML/NJ prevents that, so we go through the following convolution.
*)

structure TreeSequenceTree =
   struct
      datatype 'a seqtree =
         EMPTY 
       | ELT of 'a 
       | NODE of 'a seqtree * 'a seqtree
   end

structure TreeSequence
  :> SEQUENCE
     where type 'a seq = 'a TreeSequenceTree.seqtree
     where type 'a treeview = 'a TreeSequenceTree.seqtree
  =
struct
  exception NYI (* not yet implemented *)

  datatype treeview = datatype TreeSequenceTree.seqtree

  type 'a seq = 'a treeview                        

  fun showt t = t
  fun hidet t = t

  datatype 'a listview =
     NIL
   | CONS of ('a * 'a seq)

  type 'a ord = 'a * 'a -> order

  fun empty () = EMPTY
  
  fun singleton x = ELT x

  fun length t = case t of
                      EMPTY => 0
                    | ELT x => 1
                    | NODE (t1, t2) => length t1 + length t2

  fun root t = case t of
                    EMPTY => raise Subscript
                  | ELT x => x
                  | NODE (t1, _) => root t1

  fun nth t n = case (t, n) of
                     (ELT x, 1) => x
                   | (NODE(t1,t2), n) =>
                       let
                         val size1 = length t1
                       in
                         case (Int.compare (size1, n)) of
                              LESS => nth t1 n
                            | EQUAL => root t2
                            | GREATER => nth t2 (n - size1)
                       end
                   | _ => raise Subscript

  val append = NODE

  fun toList t =
     let
        fun loop t l =
           (case t of
               EMPTY => l
             | ELT x =>
                  x :: l
             | NODE (left, right) =>
                  loop left (loop right l))
     in
        loop t []
     end

  fun cons (h, t) = NODE (ELT h, t)

  fun foldl f b t =
     (case t of
         EMPTY => b
       | ELT x =>
            f (x, b)
       | NODE (left, right) =>
            foldl f (foldl f b left) right)

  fun foldr f b t =
     (case t of
         EMPTY => b
       | ELT x =>
            f (x, b)
       | NODE (left, right) =>
            foldl f (foldl f b right) left)

  fun iter f b s = foldl (fn (x, y) => f (y, x)) b s
  
  fun map f t =
     (case t of
         EMPTY => EMPTY
       | ELT x =>
            ELT (f x)
       | NODE (left, right) =>
            NODE (map f left, map f right))



  fun tabulate _ _ = raise NYI                   
  fun fromList _ = raise NYI
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
  fun showl _ = raise NYI
  fun hidel _ = raise NYI
end


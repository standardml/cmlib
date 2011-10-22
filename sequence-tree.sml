structure TreeSequence
  :> sig
      (* Circumvent NJ sharing bug;
      *   this should be done with "sharing type seq = treeview" *)
      type 'a seqtree
      include SEQUENCE where type 'a seq = 'a seqtree
                       where type 'a treeview = 'a seqtree
    end =
struct
  datatype 'a seq =  EMPTY 
                        | ELT of 'a 
                        | NODE of ('a seq * 'a seq) 
  type 'a seqtree = 'a seq                        

  type 'a treeview = 'a seq                        

  datatype 'a listview = NIL
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
end

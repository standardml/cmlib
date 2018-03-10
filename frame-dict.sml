
functor FrameDictFun (structure Ordered : ORDERED
                      structure Hashable : HASHABLE
                      sharing type Ordered.t = Hashable.t)
   :> FRAME_DICT
      where type key = Ordered.t
      where type init = int
   =
   struct

      structure D = SplayRDict (structure Key = Ordered)
      structure T = HashTable (structure Key = Hashable)
      
      type init = int
      type key = Ordered.t

      datatype 'a pile = Root of 'a T.table | Cons of 'a D.dict ref * 'a pile

      datatype 'a frame = RootFrame of 'a T.table | ConsFrame of 'a D.dict ref

      type 'a dict = 'a D.dict * 'a pile

      exception Absent

      fun create size =
         let val t = T.table size
         in
            (RootFrame t, (D.empty, Root t))
         end

      fun insert (d, p) key data = (D.insert d key data, p)

      fun findPile p key =
         (case p of
             Root t => T.find t key
           | Cons (ref d, p') =>
                (case D.find d key of
                    NONE => findPile p' key
                  | x as SOME _ => x))

      fun find (d, p) key =
         (case D.find d key of
             NONE => findPile p key
           | x as SOME _ => x)

      fun lookup dp key =
         (case find dp key of
             NONE => raise Absent
           | SOME x => x)

      fun addFrame (d, p) =
         let val r = ref D.empty
         in
            if D.isEmpty d then
               (ConsFrame r, (D.empty, Cons (r, p)))
            else
               (ConsFrame r, (D.empty, Cons (r, Cons (ref d, p))))
         end

      fun memberFrame frame key =
         (case frame of
             RootFrame t => T.member t key
           | ConsFrame (ref d) => D.member d key)

      fun findFrame frame key =
         (case frame of
             RootFrame t => T.find t key
           | ConsFrame (ref d) => D.find d key)

      fun insertFrame frame key data =
         (case frame of
             RootFrame t => T.insert t key data
           | ConsFrame r => r := D.insert (!r) key data)

      fun foldFrame f x frame =
         (case frame of
             RootFrame t => T.fold f x t
           | ConsFrame (ref d) => D.foldl f x d)

   end

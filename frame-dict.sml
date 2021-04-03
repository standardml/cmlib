
functor FrameDictFun (structure Ordered : ORDERED
                      structure Hashable : HASHABLE
                      sharing type Ordered.t = Hashable.t)
   :> FRAME_DICT
      where type key = Ordered.t
      where type init = int
   =
   struct

      structure D = SplayDict (structure Key = Ordered)
      structure T = HashTable (structure Key = Hashable)
      
      type init = int
      type key = Ordered.t

      datatype 'a frame = Table of 'a T.table | Dict of 'a D.dict ref

      type 'a dict = 'a D.dict * 'a frame list

      exception Absent

      val empty = (D.empty, [])

      fun insert (d, p) key data = (D.insert d key data, p)

      fun findPile p key =
         (case p of
             nil => NONE
           | Table t :: p' =>
                 (case T.find t key of
                     NONE => findPile p' key
                   | x as SOME _ => x)
           | Dict (ref d) :: p' =>
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
         let val frame = Dict (ref D.empty)
         in
            if D.isEmpty d then
               (frame, (D.empty, frame :: p))
            else
               (frame, (D.empty, frame :: Dict (ref d) :: p))
         end

      fun addFrameLarge size (d, p) =
         let val frame = Table (T.table size)
         in
            if D.isEmpty d then
               (frame, (D.empty, frame :: p))
            else
               (frame, (D.empty, frame :: Dict (ref d) :: p))
         end

      fun memberFrame frame key =
         (case frame of
             Table t => T.member t key
           | Dict (ref d) => D.member d key)

      fun findFrame frame key =
         (case frame of
             Table t => T.find t key
           | Dict (ref d) => D.find d key)

      fun insertFrame frame key data =
         (case frame of
             Table t => T.insert t key data
           | Dict r => r := D.insert (!r) key data)

      fun foldFrame f x frame =
         (case frame of
             Table t => T.fold f x t
           | Dict (ref d) => D.foldl f x d)

   end

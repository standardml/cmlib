
functor HashTable (structure Key : HASHABLE)
   :> HASH_TABLE where type key = Key.t
   =
   struct

      type key = Key.t

      datatype 'a entry =
         Nil
       | Cons of word * key * 'a ref * 'a entry ref

      (* This is a little clumsy, since the first entry in a bucket is modified
         by updating the array and the remaining entries are modified using by 
         assigning to a reference, but it's okay because we want to special-case
         the first entry anyway.
      *)

      type 'a table =
         { residents : int ref,  (* current number of residents *)
           size : word,
           thresh : int,         (* number of residents at which to resize *)
           arr : 'a entry array } ref

      exception Absent

      fun resizeLoad n = n div 3 * 4

      fun table sz =
         if sz <= 0 then
            raise (Fail "illegal size")
         else
            ref { residents = ref 0,
                  size = Word.fromInt sz,
                  thresh = resizeLoad sz,
                  arr = Array.array (sz, Nil) }

      fun reset table sz =
         if sz <= 0 then
            raise (Fail "illegal size")
         else
            table := { residents = ref 0,
                       size = Word.fromInt sz,
                       thresh = resizeLoad sz,
                       arr = Array.array (sz, Nil) }


      fun search hash key curr =
         (case !curr of
             Nil =>
                Nil
           | entry as Cons (hash', key', datumref, next) =>
                if hash = hash' andalso Key.eq (key, key') then
                   (
                   curr := !next;  (* remove from list *)
                   entry
                   )
                else
                   search hash key next)

      fun resize (table as ref { residents, size, thresh, arr, ... } : 'a table) =
         if !residents < thresh then
            ()
         else
            let
               val newsize = 2 * Word.toInt size + 1
               val newsize' = Word.fromInt newsize
               val arr' = Array.array (newsize, Nil)
               
               fun move entry =
                  (case entry of
                      Nil => ()
                    | Cons (hash, _, _, next) =>
                         let
                            val entry' = !next
                            val n = Word.toInt (hash mod newsize')
                         in
                            next := Array.sub (arr', n);
                            Array.update (arr', n, entry);
                            move entry'
                         end)
            in
               (* Move entries to new array. *)
               Array.app move arr;
            
               table := { residents = residents,
                          size = newsize',
                          thresh = resizeLoad newsize,
                          arr = arr' }
            end

      fun member (ref { size, arr, ...} : 'a table) key = 
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
            val bucket = Array.sub (arr, n)
         in
            (case bucket of
                Nil => false
              | Cons (hash', key', _, next) =>
                   (hash = hash' andalso Key.eq (key, key'))
                   orelse
                   (case search hash key next of
                       Nil => false
                     | entry as Cons (_, _, _, next') =>
                          (
                          next' := bucket;
                          Array.update (arr, n, entry);
                          true
                          )))
         end
         
      fun size (ref {residents, ...} : 'a table) = !residents

      fun insert (table as ref { residents, size, arr, ... } : 'a table) key datum =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
            val bucket = Array.sub (arr, n)
         in
            (case bucket of
                Nil =>
                   (
                   Array.update (arr, n,
                                 Cons (hash, key, ref datum, ref Nil));
                   residents := !residents + 1;
                   resize table
                   )
              | Cons (hash', key', datumref, next) =>
                   if hash = hash' andalso Key.eq (key, key') then
                      datumref := datum
                   else
                      (case search hash key next of
                          Nil =>
                             (
                             Array.update (arr, n,
                                           Cons (hash, key, ref datum, ref bucket));
                             residents := !residents + 1;
                             resize table
                             )
                        | entry as Cons (_, _, datumref', next') =>
                             (
                             next' := bucket;
                             Array.update (arr, n, entry);
                             datumref' := datum
                             )))
         end

      fun remove (table as ref { residents, size, arr, ... } : 'a table) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
            val bucket = Array.sub (arr, n)
         in
            (case bucket of
                Nil => ()
              | Cons (hash', key', _, next) =>
                   if hash = hash' andalso Key.eq (key, key') then
                      Array.update (arr, n, !next)
                   else
                      (
                      search hash key next;
                      ()
                      ))
         end

      fun find (table as ref { residents, size, arr, ... } : 'a table) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
            val bucket = Array.sub (arr, n)
         in
            (case bucket of
                Nil => NONE
              | Cons (hash', key', datumref, next) =>
                   if hash = hash' andalso Key.eq (key, key') then
                      SOME (!datumref)
                   else
                      (case search hash key next of
                          Nil => NONE
                        | entry as Cons (_, _, datumref', next') =>
                             (
                             next' := bucket;
                             Array.update (arr, n, entry);
                             SOME (!datumref')
                             )))
         end
                      
      fun lookup (table as ref { residents, size, arr, ... } : 'a table) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
            val bucket = Array.sub (arr, n)
         in
            (case bucket of
                Nil =>
                   raise Absent
              | Cons (hash', key', datumref, next) =>
                   if hash = hash' andalso Key.eq (key, key') then
                      !datumref
                   else
                      (case search hash key next of
                          Nil =>
                             raise Absent
                        | entry as Cons (_, _, datumref', next') =>
                             (
                             next' := bucket;
                             Array.update (arr, n, entry);
                             !datumref'
                             )))
         end

      fun operate (table as ref { residents, size, arr, ... } : 'a table) key absentf presentf =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
            val bucket = Array.sub (arr, n)
         in
            (case bucket of
                Nil =>
                   let
                      val datum = absentf ()
                   in
                      Array.update (arr, n,
                                    Cons (hash, key, ref datum, ref Nil));
                      residents := !residents + 1;
                      resize table;
                      (NONE, datum)
                   end
              | Cons (hash', key', datumref, next) =>
                   if hash = hash' andalso Key.eq (key, key') then
                      let
                         val datum = !datumref
                         val datum' = presentf datum
                      in
                         datumref := datum';
                         (SOME datum, datum')
                      end
                   else
                      (case search hash key next of
                          Nil =>
                             let
                                val datum = absentf ()
                             in
                                Array.update (arr, n,
                                              Cons (hash, key, ref datum, ref bucket));
                                residents := !residents + 1;
                                resize table;
                                (NONE, datum)
                             end
                        | entry as Cons (_, _, datumref', next') =>
                             let
                                val datum = !datumref'
                                val datum' = presentf datum
                             in
                                next' := bucket;
                                Array.update (arr, n, entry);
                                datumref' := datum';
                                (SOME datum, datum')
                             end))
         end

      fun insertMerge table key x f =
         (
         operate table key (fn () => x) f;
         ()
         )

      fun lookupOrInsert table key datumf =
         #2 (operate table key datumf (fn x => x))

      fun lookupOrInsert' table key datumf =
         let
            val (old, datum) = operate table key datumf (fn x => x)
         in
            (datum, isSome old)
         end

      fun foldEntry f x entry =
         (case entry of
             Nil => x
           | Cons (_, key, ref datum, ref next) =>
                foldEntry f (f (key, datum, x)) next)

      fun fold f x (ref { arr, ... } : 'a table) =
         Array.foldl
         (fn (bucket, acc) => foldEntry f acc bucket)
         x
         arr

      fun toList table =
         fold (fn (key, datum, l) => (key, datum) :: l) [] table

      fun appEntry f entry =
         (case entry of
             Nil => ()
           | Cons (_, key, ref datum, ref next) =>
                (
                f (key, datum);
                appEntry f next
                ))

      fun app f (ref { arr, ... } : 'a table) =
         Array.app (appEntry f) arr
         

   end

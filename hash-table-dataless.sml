
functor DatalessHashTable (structure Key : HASHABLE)
   :>
   DATALESS_HASH_TABLE
   where type key = Key.t
   =
   struct

      type init = int
      type key = Key.t

      datatype entry =
         Nil
       | Cons of word * key * entry ref

      datatype bucket =
         Zero
       | One of key
       | Many of entry ref  (* invariant: length >=2 *)

      type table =
         { residents : int ref,  (* current number of residents *)
           size : word,
           thresh : int,         (* number of residents at which to resize *)
           arr : bucket array } ref

      exception Absent

      fun resizeLoad n = n div 4 * 3

      fun initial sz =
         if sz <= 0 then
            raise (Fail "illegal size")
         else
            { residents = ref 0,
              size = Word.fromInt sz,
              thresh = resizeLoad sz,
              arr = Array.array (sz, Zero) }

      fun table sz = ref (initial sz)

      fun reset table sz =
         table := initial sz

      fun size (ref { residents, ... } : table) = !residents


      fun findEntry lr hash key =
         (case !lr of
             Nil =>
                lr
           | Cons (hash', key', rest) =>
                if hash = hash' andalso Key.eq (key, key') then
                   lr
                else
                   findEntry rest hash key)


      fun resize (table as ref { residents, size, thresh, arr, ... } : table) =
         if !residents < thresh then
            ()
         else
            let
               val newsize = 2 * Word.toInt size
               val newsize' = Word.fromInt newsize
               val arr' = Array.array (newsize, Zero)
               
               fun moveInto hash key =
                  let
                     val n = Word.toInt (hash mod newsize')
                  in
                     (case Array.sub (arr', n) of
                         Zero =>
                            Array.update (arr', n, One key)
                       | One key' =>
                            Array.update (arr', n,
                                          Many (ref (Cons (hash, key,
                                                           ref (Cons (Key.hash key', key',
                                                                      ref Nil))))))
                       | Many lr =>
                            Array.update (arr', n, Many (ref (Cons (hash, key, lr)))))
                  end

               fun moveList lr =
                  (case !lr of
                      Nil => ()
                    | Cons (hash, key, rest) =>
                         (
                         moveInto hash key;
                         moveList rest
                         ))

               fun move bucket =
                  (case bucket of
                      Zero => ()
                    | One key =>
                         moveInto (Key.hash key) key
                    | Many lr =>
                         moveList lr)
            in
               (* Move entries to new array. *)
               Array.app move arr;
            
               table := { residents = residents,
                          size = newsize',
                          thresh = resizeLoad newsize,
                          arr = arr' }
            end


      fun member (ref { size, arr, ... } : table) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
         in
            (case Array.sub (arr, n) of
                Zero => false
              | One key' =>
                   Key.eq (key, key')
              | Many lr =>
                   (case !(findEntry lr hash key) of
                       Nil => false
                     | Cons _ => true))
         end


      fun swap (table as ref { residents, size, arr, ... } : table) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
         in
            (case Array.sub (arr, n) of
                Zero =>
                   (
                   Array.update (arr, n, One key);
                   residents := !residents + 1;
                   resize table;
                   NONE
                   )
              | One key' =>
                   if Key.eq (key, key') then
                      (
                      Array.update (arr, n, One key);
                      SOME key'
                      )
                   else
                      (
                      Array.update (arr, n,
                                    Many (ref (Cons (hash, key,
                                                     ref (Cons (Key.hash key', key',
                                                                ref Nil))))));
                      residents := !residents + 1;
                      resize table;
                      NONE
                      )
              | Many lr =>
                   (case findEntry lr hash key of
                       ref Nil =>
                          (
                          Array.update (arr, n,
                                        Many (ref (Cons (hash, key, lr))));
                          residents := !residents + 1;
                          resize table;
                          NONE
                          )
                     | lr' as ref (Cons (_, key', rest)) =>
                          (
                          lr' := Cons (hash, key, rest);
                          SOME key'
                          )))
         end

      fun insert table key = (swap table key; ())


      fun remove (table as ref { residents, size, arr, ... } : table) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
         in
            (case Array.sub (arr, n) of
                Zero => ()
              | One key' =>
                   if Key.eq (key, key') then
                      (
                      Array.update (arr, n, Zero);
                      residents := !residents - 1
                      )
                   else
                      ()
              | Many lr =>
                   (case findEntry lr hash key of
                       ref Nil => ()
                     | lr' as ref (Cons (_, _, rest)) =>
                          (
                          lr' := !rest;
                          (case !lr of
                              Cons (_, key', ref Nil) =>
                                 Array.update (arr, n, One key')
                            | Nil =>
                                 raise (Fail "invariant")
                            | _ => ());
                          residents := !residents - 1
                          )))
         end


      fun lookup (table as ref { size, arr, ... } : table) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
         in
            (case Array.sub (arr, n) of
                Zero =>
                   raise Absent
              | One key' =>
                   if Key.eq (key, key') then
                      key'
                   else
                      raise Absent
              | Many lr =>
                   (case findEntry lr hash key of
                       ref Nil =>
                          raise Absent
                     | ref (Cons (_, key', _)) =>
                          key'))
         end


      fun find table key =
         (SOME (lookup table key)
          handle Absent => NONE)


      fun lookupOrInsert' (table as ref { residents, size, arr, ... } : table) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (hash mod size)
         in
            (case Array.sub (arr, n) of
                Zero =>
                   (
                   Array.update (arr, n, One key);
                   residents := !residents + 1;
                   resize table;
                   (key, false)
                   )
              | One key' =>
                   if Key.eq (key, key') then
                      (key', true)
                   else
                      (
                      Array.update (arr, n,
                                    Many (ref (Cons (hash, key,
                                                     ref (Cons (Key.hash key', key',
                                                                ref Nil))))));
                      residents := !residents + 1;
                      resize table;
                      (key, false)
                      )
              | Many lr =>
                   (case findEntry lr hash key of
                       ref Nil =>
                          (
                          Array.update (arr, n,
                                        Many (ref (Cons (hash, key, lr))));
                          residents := !residents + 1;
                          resize table;
                          (key, false)
                          )
                     | lr' as ref (Cons (_, key', rest)) =>
                          (key', true)))
         end

      fun lookupOrInsert table key = #1 (lookupOrInsert' table key)         


      fun foldEntry f x entry =
         (case entry of
             Nil => x
           | Cons (_, key, ref next) =>
                foldEntry f (f (key, x)) next)

      fun fold f x (ref { arr, ... } : table) =
         Array.foldl
         (fn (Zero, acc) => acc
           | (One key, acc) => f (key, acc)
           | (Many (ref l), acc) => foldEntry f acc l)
         x
         arr

      fun foldEntryLazy f rem entry =
         (case entry of
             Nil => Susp.force rem
           | Cons (_, key, ref next) =>
                f (key,
                   Susp.delay (fn () => foldEntryLazy f rem next)))

      fun foldLazy f x (ref { arr, ... } : table) =
         ArrayUtil.foldlLazy
         (fn (Zero, rem) => Susp.force rem
           | (One key, rem) => f (key, rem)
           | (Many (ref l), rem) => foldEntryLazy f rem l)
         x
         arr

      fun toList table =
         fold (fn (key, l) => key :: l) [] table

      fun appEntry f entry =
         (case entry of
             Nil => ()
           | Cons (_, key, ref next) =>
                (
                f key;
                appEntry f next
                ))

      fun app f (ref { arr, ... } : table) =
         Array.app 
         (fn Zero => ()
           | One key => f key
           | Many (ref l) => appEntry f l)
         arr

   end

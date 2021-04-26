
functor HashTable (structure Key : HASHABLE)
   :> HASH_TABLE where type key = Key.t
   =
   struct

      type init = int
      type key = Key.t

      datatype 'a entry =
         Nil
       | Cons of word * key * 'a * 'a entry ref

      datatype 'a bucket =
         Zero
       | One of key * 'a
       | Many of 'a entry ref  (* invariant: length >=2 *)


      datatype 'a pretable =
         T of { residents : int ref,  (* current number of residents *)
                size : word,
                thresh : int,         (* number of residents at which to resize *)
                arr : 'a bucket array }

      type 'a table = 'a pretable ref

      exception Absent

      fun resizeLoad n = n div 4 * 3

      fun initial sz =
         if sz <= 0 then
            raise (Fail "illegal size")
         else
            T { residents = ref 0,
                size = Word.fromInt sz,
                thresh = resizeLoad sz,
                arr = Array.array (sz, Zero) }

      fun table sz = ref (initial sz)

      fun reset table sz =
         table := initial sz

      fun size (ref (T { residents, ... })) = !residents


      fun findEntry lr hash key =
         (case !lr of
             Nil =>
                lr
           | Cons (hash', key', _, rest) =>
                if hash = hash' andalso Key.eq (key, key') then
                   lr
                else
                   findEntry rest hash key)


      fun resize (table as ref (T { residents, size, thresh, arr, ... })) =
         if !residents < thresh then
            ()
         else
            let
               val newsize = 2 * Word.toInt size
               val newsize' = Word.fromInt newsize
               val arr' = Array.array (newsize, Zero)
               
               fun moveInto hash key datum =
                  let
                     val n = Word.toInt (Word.mod (hash, newsize'))
                  in
                     (case Array.sub (arr', n) of
                         Zero =>
                            Array.update (arr', n, One (key, datum))
                       | One (key', datum') =>
                            Array.update (arr', n,
                                          Many (ref (Cons (hash, key, datum,
                                                           ref (Cons (Key.hash key', key', datum',
                                                                      ref Nil))))))
                       | Many lr =>
                            Array.update (arr', n, Many (ref (Cons (hash, key, datum, lr)))))
                  end

               fun moveList lr =
                  (case !lr of
                      Nil => ()
                    | Cons (hash, key, datum, rest) =>
                         (
                         moveInto hash key datum;
                         moveList rest
                         ))

               fun move bucket =
                  (case bucket of
                      Zero => ()
                    | One (key, datum) =>
                         moveInto (Key.hash key) key datum
                    | Many lr =>
                         moveList lr)
            in
               (* Move entries to new array. *)
               Array.app move arr;
            
               table := T { residents = residents,
                            size = newsize',
                            thresh = resizeLoad newsize,
                            arr = arr' }
            end


      fun member (ref (T { size, arr, ... })) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (Word.mod (hash, size))
         in
            (case Array.sub (arr, n) of
                Zero => false
              | One (key', _) =>
                   Key.eq (key, key')
              | Many lr =>
                   (case !(findEntry lr hash key) of
                       Nil => false
                     | Cons _ => true))
         end


      fun insert (table as ref (T { residents, size, arr, ... })) key datum =
         let
            val hash = Key.hash key
            val n = Word.toInt (Word.mod (hash, size))
         in
            (case Array.sub (arr, n) of
                Zero =>
                   (
                   Array.update (arr, n,
                                 One (key, datum));
                   residents := !residents + 1;
                   resize table
                   )
              | One (key', datum') =>
                   if Key.eq (key, key') then
                      Array.update (arr, n, One (key, datum))
                   else
                      (
                      Array.update (arr, n,
                                    Many (ref (Cons (hash, key, datum,
                                                     ref (Cons (Key.hash key', key', datum',
                                                                ref Nil))))));
                      residents := !residents + 1;
                      resize table
                      )
              | Many lr =>
                   (case findEntry lr hash key of
                       ref Nil =>
                          (
                          Array.update (arr, n,
                                        Many (ref (Cons (hash, key, datum, lr))));
                          residents := !residents + 1;
                          resize table
                          )
                     | lr' as ref (Cons (_, _, _, rest)) =>
                          lr' := Cons (hash, key, datum, rest)))
         end


      fun remove (ref (T { residents, size, arr, ... })) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (Word.mod (hash, size))
         in
            (case Array.sub (arr, n) of
                Zero => ()
              | One (key', _) =>
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
                     | lr' as ref (Cons (_, _, _, rest)) =>
                          (
                          lr' := !rest;
                          (case !lr of
                              Cons (_, key', datum', ref Nil) =>
                                 Array.update (arr, n, One (key', datum'))
                            | Nil =>
                                 raise (Fail "invariant")
                            | _ => ());
                          residents := !residents - 1
                          )))
         end


      fun lookup (ref (T { size, arr, ... })) key =
         let
            val hash = Key.hash key
            val n = Word.toInt (Word.mod (hash, size))
         in
            (case Array.sub (arr, n) of
                Zero =>
                   raise Absent
              | One (key', datum) =>
                   if Key.eq (key, key') then
                      datum
                   else
                      raise Absent
              | Many lr =>
                   (case findEntry lr hash key of
                       ref Nil =>
                          raise Absent
                     | ref (Cons (_, _, datum, _)) =>
                          datum))
         end


      fun find table key =
         (SOME (lookup table key)
          handle Absent => NONE)


      fun operate' (table as ref (T { residents, size, arr, ... })) key absentf presentf =
         let
            val hash = Key.hash key
            val n = Word.toInt (Word.mod (hash, size))
         in
            (case Array.sub (arr, n) of
                Zero =>
                   let
                      val datumo = absentf ()
                   in
                      (case datumo of
                          NONE => ()
                        | SOME datum =>
                             (
                             Array.update (arr, n, One (key, datum));
                             residents := !residents + 1;
                             resize table
                             ));
                      (NONE, datumo)
                   end
              | One (key', datum') =>
                   if Key.eq (key, key') then
                      let
                         val datumo = presentf datum'
                      in
                         (case datumo of
                             NONE =>
                                (
                                Array.update (arr, n, Zero);
                                residents := !residents - 1
                                )
                           | SOME datum =>
                                Array.update (arr, n, One (key, datum)));
                         (SOME datum', datumo)
                      end
                   else
                      let
                         val datumo = absentf ()
                      in
                         (case datumo of
                             NONE =>
                                ()
                           | SOME datum =>
                                (
                                Array.update (arr, n,
                                              Many (ref (Cons (hash, key, datum,
                                                               ref (Cons (Key.hash key', key', datum',
                                                                          ref Nil))))));
                                residents := !residents + 1;
                                resize table
                                ));
                         (NONE, datumo)
                      end
              | Many lr =>
                   (case findEntry lr hash key of
                       ref Nil =>
                          let
                             val datumo = absentf ()
                          in
                             (case datumo of
                                 NONE => ()
                               | SOME datum =>
                                    (
                                    Array.update (arr, n,
                                                  Many (ref (Cons (hash, key, datum, lr))));
                                    residents := !residents + 1;
                                    resize table
                                    ));
                             (NONE, datumo)
                          end
                     | lr' as ref (Cons (_, _, datum', rest)) =>
                          let
                             val datumo = presentf datum'
                          in
                             (case datumo of
                                 NONE =>
                                    (
                                    lr' := !rest;
                                    (case !lr of
                                        Cons (_, key'', datum'', ref Nil) =>
                                           Array.update (arr, n, One (key'', datum''))
                                      | Nil =>
                                           raise (Fail "invariant")
                                      | _ => ());
                                    residents := !residents - 1
                                    )
                               | SOME datum =>
                                    lr' := Cons (hash, key, datum, rest));
                             (SOME datum', datumo)
                          end))
         end


      fun operate table key absentf presentf =
         let
            val (x, y) = operate' table key (SOME o absentf) (SOME o presentf)
         in
            (x, valOf y)
         end

      fun insertMerge table key x f =
         let
            val _ = operate' table key (fn () => SOME x) (SOME o f)
         in
            ()
         end

      fun lookupOrInsert table key datumf =
         let
            val (x, y) = operate' table key (SOME o datumf) (fn x => SOME x)
         in
            valOf y
         end

      fun lookupOrInsert' table key datumf =
         let
            val (x, y) = operate' table key (SOME o datumf) (fn x => SOME x)
         in
            (valOf y, isSome x)
         end


      fun foldEntry f x entry =
         (case entry of
             Nil => x
           | Cons (_, key, datum, ref next) =>
                foldEntry f (f (key, datum, x)) next)

      fun fold f x (ref (T { arr, ... })) =
         Array.foldl
         (fn (Zero, acc) => acc
           | (One (key, datum), acc) => f (key, datum, acc)
           | (Many (ref l), acc) => foldEntry f acc l)
         x
         arr

      fun toList table =
         fold (fn (key, datum, l) => (key, datum) :: l) [] table

      fun appEntry f entry =
         (case entry of
             Nil => ()
           | Cons (_, key, datum, ref next) =>
                (
                f (key, datum);
                appEntry f next
                ))

      fun app f (ref (T { arr, ... })) =
         Array.app 
         (fn Zero => ()
           | One (key, datum) => f (key, datum)
           | Many (ref l) => appEntry f l)
         arr

   end

structure ArraySequence : SEQUENCE
=
struct
  type 'a seq = 'a array
  type 'a ord = 'a * 'a -> order

  exception NYI

  (* helper functions used throughout *)
  (* eval x to r, run y on r for effect, eval to r *)
  fun >> (x,y) =
      let
        val res = x
        val _ = y res
      in
        res
      end
  infix >>

  (* increments an int reference *)
  fun add x r = (r := (!r) + x)
  val inc = add 1

  (* flips argument order to a tupled function *)
  fun flip f (x,y) = f (y,x)


  val length = Array.length

  val fromList = Array.fromList
  val % = fromList

  fun tabulate f n = Array.tabulate (n,f)

  fun nth s i = Array.sub(s,i)
      handle Subscript => raise Subscript

  fun singleton x = Array.fromList [x]

  fun empty _ = Array.fromList []

  fun map f s = tabulate (f o (nth s)) (length s)

  fun map2 f s1 s2 = tabulate (fn i => f (nth s1 i, nth s2 i))
                              (Int.min(length s1, length s2))

  fun iter_gen fold = fold o flip

  fun foldh_gen foldi f b s =
      case length s
       of 0 => (empty (), b)
        | 1 => (singleton b, f(nth s 0, b))
        | n =>
          let
            (* TODO: (b :'a), so this is grossly space inefficient *)
            val p_res = Array.array (n,b)

            fun f' (idx,a,b) =
                let
                  val _ = Array.update(p_res, idx, b)
                in
                  f (a,b)
                end

            val res = foldi f' b s
          in
            (p_res, res)
          end

  val foldl = Array.foldl
  val foldr = Array.foldr

  fun foldlh f b s = foldh_gen Array.foldli f b s

  fun iter f b s = Array.foldl (flip f) b s

  fun iterh f b s = foldh_gen Array.foldli (flip f) b s

  fun red act f b s =
      case length s
       of 0 => b
        | _ =>
          let
            (* Word.toint, Word.fromInt less horrible hack / faster?*)
            fun ppt x =
                let
                  fun ppt' (curr,prev) =
                      case (Int.compare (curr,x))
                       of LESS => ppt' (2*curr,curr)
                        | _ => prev
                in
                  ppt' (1,0)
                end

            (* using int * int as INCLUSIVE indices *)
            fun l (a,b) = (a,(a + (ppt ( (b-a) + 1)))-1)
            fun r (a,b) = (a + (ppt ((b-a) + 1)), b)

            fun red' (p as (i,j)) =
                case (j - i)
                 of 0 => nth s i
                  | _ => f(red' (l p), red' (r p))  >>  act
          in
            f(b, red' (0, (length s) - 1))
          end

  fun reduce f = red ignore f

  (* Make this parallel *)
  fun packIndex p s =
      let
	  val l = length s
          val indices = Array.array(l,0)
          fun scan (i,m) =
	      if (i = l) then m
	      else
                  if (p (nth s i))
		  then (Array.update(indices, m, i); scan(i+1, m+1))
		  else scan(i+1, m)
	  val m = scan(0,0)
      in
	  (indices, m)
      end

  fun filter p s =
      let
	  val (indices, m) = packIndex p s
	  fun extract i = nth s (nth indices i)
      in
          tabulate extract m
      end

  exception internalError

  fun filterSome s =
      let
	  fun isSome NONE = false | isSome (SOME(_)) = true
	  fun exSome (SOME(x)) = x | exSome NONE = raise internalError
	  val (indices, m) = packIndex isSome s
	  fun extract i = exSome(nth s (nth indices i))
      in
          tabulate extract m
      end

  fun mapFilter f s = filterSome (map f s)
  fun tabFilter f n = filterSome (tabulate f n)

  (* TODO: rewrite? this is really bizarre to read *)
  fun flatten ss =
      case Array.foldl (fn (s,part) => (length s) + part) 0 ss
       of 0 => empty()
        | n =>
          let
            val ind_pairs = Array.array(n, (0,0))

            fun run start 0 _ = ()
              | run start len lcomp =
                (Array.update(ind_pairs,start+len-1,(lcomp,len-1));
                 run start (len - 1) lcomp)

            fun proc (i, s, prev) =
                let
                  val n = length s
                  val _ = run prev n i
                in
                  (prev + n)
                end

            val _ = Array.foldli proc 0 ss

            fun grab i =
                let
                  val (out,inn) = nth ind_pairs i
                in
                  nth (nth ss out) inn
                end
          in
            tabulate grab n
          end

  fun partition parts s =
      let
        (* TODO: to make this parallelizable, you need to precompute the
        sequence of partial sums of parts so that the inner tabulate starts
        at the right place in s. this version is simpler and better for the
        sequential case.
         *)
        val idx = ref 0
      in
        tabulate (fn i => tabulate
                              (fn _ => nth s (!idx before inc idx))
                              (nth parts i))
                 (length parts)
      end

  fun inject ins s =
      tabulate (nth s) (length s)
               >> (fn R => Array.app (fn (i,x) => Array.update(R,i,x)) ins)

  fun append (s1,s2) =
      let
        val l1 = length s1
        val l2 = length s2

        fun grab x =
            case Int.compare (x,l1)
             of LESS => nth s1 x
              | _ =>    nth s2 (x - l1)
      in
        tabulate grab (l1+l2)
      end

  fun rake _ _ = raise NYI

  fun take (s, i) = tabulate (nth s) i

  fun drop (s,i) = tabulate (fn x => nth s (x + i)) ((length s) - i)

  fun subseq s (j, len) = tabulate (fn i => nth s (j+i)) len

  fun splitMid (s, i) =
      case (length s)
       of 0 => NONE
        | _ => SOME (take (s, i), nth s i, drop (s, i+1))

  fun sort ord s =
      let
        fun quick s =
            case length s
             of 0 => empty ()
              | 1 => singleton (nth s 0)
              | n =>
                let
		  (* The following pivot is not theoretically good,
		   * but ok in practice *)
                  val pivot = nth s (n div 2)
                  val L = filter (fn a => ord (a,pivot) = LESS) s
                  val E = filter (fn a => ord (a,pivot) = EQUAL) s
                  val G = filter (fn a => ord (a,pivot) = GREATER) s
                in
                  flatten (fromList [quick L, E, quick G])
                end
      in
        quick s
      end

  (* TODO: rewrite this so it makes sense to read. look and say? *)
  fun collect ord s =
      case length s
       of 0 => empty ()
        | _ =>
          let
            (* can't modify argument sequence *)
            val s' = sort (fn ((a,_),(b,_)) => ord(a,b)) s

            (* left projection of an index into s' *)
            fun lp_sub i = #1 (nth s' i)

            (* count the number of unique alphas *)
            fun cnt ((x,_),(prev,acc)) =
                (x, acc + (case ord(x,prev)
                            of EQUAL => 0
                             | _ => 1))
            val (_,uniq) = Array.foldl cnt (lp_sub 0,1) s'

            (* number of adjacent occurences of what ever is at idx *)
            fun run_len idx =
                let
                  val x = lp_sub idx
                  val len = length s'

                  fun rl i =
                      (* TODO: worth it to avoid compare? *)
                      case (ord(x, lp_sub i), Int.compare(i, len - 1))
                       of (EQUAL,LESS) => 1 + rl (i+1)
                        | (EQUAL,EQUAL) => 1
                        | _ => 0
                in
                  rl idx
                end

            (* index into argument sequence *)
            val idx = ref 0

            (* conscutive betas starting at i whose paired alphas are eq *)
            fun betaseq i =
                let
                  val len = run_len i
                  val _ = add len idx
                in
                  tabulate (fn d => #2 (nth s' (i + d))) len
                end
          in
            tabulate (fn _ => (lp_sub (!idx), betaseq (!idx))) uniq
          end

  fun toString a2s s =
      "<" ^ (String.concatWith "," (List.tabulate(length s, a2s o nth s))) ^ ">"

  fun fields cp s =
      let
	  val n = String.size s
	  val starts = tabFilter (fn i => if (cp(String.sub(s,i)))
					then SOME(i) else NONE)
				 n
	  val m = length starts
	  fun extract i =
	      let
		  val si = if (i=0) then 0 else (nth starts (i-1))+1
		  val ei = if (i=m) then n else (nth starts i)
	      in
		  String.substring(s, si, ei-si)
	      end
      in
	  tabulate extract ((length starts) + 1)
      end

  fun tokens cp s =
      filter (fn x => String.size(x) > 0) (fields cp s)

  fun fields cp s = fromList (String.fields cp s)
  fun tokens cp s = fromList (String.tokens cp s)

  datatype 'a treeview = EMPTY
                       | ELT of 'a
                       | NODE of ('a seq * 'a seq)

  fun showti s f =
      case length s
       of 0 => EMPTY
        | 1 => ELT (nth s 0)
        | n => NODE (take(s, f n), drop(s, f n))

  fun showt s = showti s (fn n => n div 2)

  fun hidet (EMPTY) = empty ()
    | hidet (ELT x) = singleton x
    | hidet (NODE (s1, s2)) = append (s1,s2)


  datatype 'a listview = NIL
                       | CONS of ('a * 'a seq)

  fun showl s =
      case (length s)
       of 0 => NIL
        | n => CONS (nth s 0, tabulate ((nth s) o (fn x => x+1)) (length s - 1))

  fun hidel NIL = empty ()
    | hidel (CONS (x,xs)) =
      let
        fun grab 0 = x
          | grab i = nth xs (i-1)
      in
        tabulate grab (1 + (length xs))
      end

  fun scan f b s =
      case length s
       of 0 => (empty(), b)
        | 1 => (singleton b, f(b, nth s 0))
        | n =>
          let
            val x = tabulate 
			(fn i => case i = (n div 2) of
				     true => (nth s (2*i))
   				   | _ => f(nth s (2*i), nth s (2*i + 1)))
                        (((n-1) div 2)+1)
            val (y, r) = scan f b x
          in
            (tabulate (fn i => case (i mod 2) of
				   0 => (nth y (i div 2))
				 | _ => f(nth y (i div 2), nth s (i-1))) 
		      n, r)
          end

  fun scanOld f b s = iter f b s

  fun merge cmp s t = sort cmp (hidet (NODE(s,t)))

  fun cons (h, t) = append (singleton h, t)

  fun toList a = Array.foldr (op ::) [] a

  fun collate _ = raise NYI

end
structure A = ArraySequence

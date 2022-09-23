
structure ParseCommandLine :> PARSE_COMMAND_LINE =
   struct

      type 'a parser = string list -> 'a * string list

      exception Error of string
      exception Backtrack of int

      fun return x l = (x, l)

      fun string l =
         (case l of
             nil => raise (Backtrack 0)

           | str :: rest => (str, rest))

      fun reject l = raise (Backtrack 0)

      fun error p l =
         let
            val (err, _) = p l
         in
            raise (Error err)
         end

      fun seq p q l1 =
         let
            val (x, l2) = p l1
            val (y, l3) = q l2
         in
            ((x, y), l3)
         end

      fun seq3 p q r l1 =
         let
            val (x, l2) = p l1
            val (y, l3) = q l2
            val (z, l4) = r l3
         in
            ((x, y, z), l4)
         end

      fun seq4 p q r s l1 =
         let
            val (x, l2) = p l1
            val (y, l3) = q l2
            val (z, l4) = r l3
            val (w, l5) = s l4
         in
            ((x, y, z, w), l5)
         end

      fun or ps l =
         let
            fun loop ps =
               (case ps of
                   nil => raise (Backtrack 0)

                 | p :: rest =>
                      (p l
                       handle (Backtrack i) => 
                          if i = 0 then
                             loop rest
                          else
                             raise (Backtrack (i-1))))
         in
            loop ps
         end

      fun map f p l =
         let
            val (x, l') = p l
         in
            (f x, l')
         end

      fun fold f z p l =
         (let
             val (x, l') = p l
          in
             fold f (f (x, z)) p l'
          end
          handle (Backtrack i) =>
             if i = 0 then
                (z, l)
             else
                raise (Backtrack (i-1)))

      fun test f p l =
         (case l of
             nil => raise (Backtrack 0)
             
           | str :: rest =>
                if f str then
                   p l
                else
                   raise (Backtrack 0))

      fun eof l =
         (case l of
             nil => ((), nil)

           | _ :: _ => raise (Backtrack 0))

      fun cut p l =
         (p l
          handle (Backtrack i) => raise (Backtrack (i+1)))

      fun parse p l =
         ((case p l of
              (x, nil) => x
 
            | (_, str :: _) => raise (Error ("unexpected argument " ^ str)))
          handle (Backtrack _) => raise (Error "cannot parse command-line arguments"))
             

      fun backtrack () = raise (Backtrack 0)


      
      structure D = RedBlackDict (structure Key = IntOrdered)

      type log = exn D.dict

      type ('a, 'b) key = ('a -> log -> log) * (log -> 'b)

      datatype ('a, 'b) machine =
         Machine of { write : 'a -> ('a, 'b) machine,
                      read  : log -> 'b }

      val blank = D.empty

      fun write (wr, _) = wr
      fun read (_, rd) = rd

      fun mapKeyIn f (wr, rd) = ((fn x => wr (f x)), rd)
      fun mapKeyOut f (wr, rd) = (wr, (fn log => f (rd log)))


      val cur = ref 0

      fun ('a, 'b) key (m : ('a, 'b) machine) =
         let
            val i = !cur
            val () = cur := i + 1

            exception E of ('a, 'b) machine

            fun get log =
               (case D.find log i of
                   NONE => m

                 | SOME (E m') => m'

                 | SOME _ =>
                      raise (Fail "impossible"))

            fun wr x log =
               let
                  val Machine {write, ...} = get log
               in
                  D.insert log i (E (write x))
               end

            fun rd log =
               let
                  val Machine {read, ...} = get log
               in
                  read log
               end
         in
            (wr, rd)
         end



      fun stringToBool str =
         (case str of
             "true" => true
           | "false" => false
           | "yes" => true
           | "no" => false
           | "on" => true
           | "off" => false
           | _ => raise (Backtrack 0))

      fun stringToInt str =
         (case FromString.toIntM str of
             NONE => raise (Backtrack 0)
           | SOME x => x)

      fun stringToNonneg str =
         let val i = stringToInt str
         in
            if i < 0 then
               raise (Backtrack 0)
            else
               i
         end



      fun expects str1 str2 = String.concat [str2, " expects ", str1]
      fun unexpected str = "unexpected argument " ^ str
      fun unrecognized str = "unrecognized option " ^ str



      fun eq (x : string) y = x = y

      fun forget p = map (fn _ => ()) p

      fun match f = test f (forget string)

      fun seqfst p q = map (fn (x, y) => x) (seq p q)

      fun seqsnd p q = map (fn (x, y) => y) (seq p q)

      val seqf = seqsnd

      fun seql ps = List.foldl (fn (p, q) => map (op ::) (seq p q)) (return []) ps

      fun trap err p = or [p, error (return err)]

      fun mapCompose f p = map (fn g => fn x => f (g x)) p

      fun foldCompose p = fold (op o) (fn x => x) p

      fun mapw k p = map (write k) p

      val bool = map stringToBool string

      val int = map stringToInt string

      val nonneg = map stringToNonneg string

      val nohyph = or [test (fn str => String.size str > 0 andalso String.sub (str, 0) = #"-") (error (map unrecognized string)), string]

      fun exactly (str : string) = match (fn str' => str = str')

      fun unitOpt str k v = map (fn () => write k v) (exactly str)

      fun stringOpt str k = seqf (exactly str) (trap (expects "an argument" str) (map (write k) nohyph))

      fun boolOpt str k = seqf (exactly str) (trap (expects "a boolean argument" str) (map (write k) bool))

      fun intOpt str k = seqf (exactly str) (trap (expects "a numeric argument" str) (map (write k) int))

      fun nonnegOpt str k = seqf (exactly str) (trap (expects "a nonnegative argument" str) (map (write k) nonneg))

      fun default x =
         Machine { write = default, read = (fn _ => x) }
         
      fun default' f =
         Machine { write = default, read = f }

      fun optionWrite x =
         Machine { write = optionWrite, read = (fn _ => SOME x) }

      val option =
         Machine { write = optionWrite, read = (fn _ => NONE) }
         
      fun listWrite l x =
         Machine { write = listWrite (x :: l),
                   read = (fn _ => rev (x :: l)) }

      val list =
         Machine { write = (fn x => listWrite [] x), read = (fn _ => []) }

      fun listDefault x =
         Machine { write = (fn x => listWrite [] x), read = (fn _ => x) }

      fun listDefault' f =
         Machine { write = (fn x => listWrite [] x), read = f }

      fun revappend l1 l2 = List.foldl (op ::) l2 l1

      fun appendWrite l1 l2 =
         Machine { write = appendWrite (revappend l2 l1),
                   read = (fn _ => rev (revappend l2 l1)) }

      val append =
         Machine { write = (fn l => appendWrite [] l), read = (fn _ => []) }

      fun appendDefault x =
         Machine { write = (fn l => appendWrite [] l), read = (fn _ => x) }

      fun appendDefault' f =
         Machine { write = (fn l => appendWrite [] l), read = f }

      fun mapIn f (Machine { write, read }) =
         let
            fun wt x = mapIn f (write (f x))
         in
            Machine {write=wt, read=read}
         end

      fun mapOut f (Machine { write, read }) =
         let
            fun wt x = mapOut f (write x)
            fun rd log = f (read log)
         in
            Machine {write=wt, read=rd}
         end
               

      type 'a id_key = ('a, 'a) key
      type 'a option_key = ('a, 'a option) key
      type 'a list_key = ('a, 'a list) key

      fun limit i err (Machine {write, read}) =
         let
            fun wt x =
               if i <= 0 then
                  raise (Error err)
               else
                  limit (i-1) err (write x)
         in
            Machine {write=wt, read=read}
         end

      fun once err = limit 1 err

   end

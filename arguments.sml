
structure Arguments :> ARGUMENTS =
   struct

      structure Parsing =
         ParsingFun (type token = string
                     structure Streamable =
                        MonomorphizeStreamable (type elem = string
                                                structure Streamable = ListStreamable))

      open Parsing

      exception Usage
      exception Exit

      fun is str = test_ (fn str' => str = str')

      val int = wrap (fn str => (case FromString.toInt str of SOME x => x | NONE => raise Usage)) accept
      val string = accept

      fun eol s =
         (case s of
             [] => ((), [])
           | _ :: _ => raise Usage)

      fun exec f = wrap f (return ())

      fun call f = wrap f accept

      fun assign r p = wrap (fn x => r := x) p
      fun set r = exec (fn () => r := true)
      fun clear r = exec (fn () => r := false)

      fun full (str : string) p s =
         (case s of
             [] =>
                raise SyntaxError
           | str' :: s' =>
                if str = str' then
                   p s'
                else
                   raise SyntaxError)

      fun prefix str p s =
         (case s of
             [] =>
                raise SyntaxError
           | str' :: s' =>
                if String.isPrefix str str' then
                   p (String.extract (str', String.size str, NONE) :: s')
                else
                   raise SyntaxError)

      fun prefix' str p s =
         (case s of
             [] =>
                raise SyntaxError
           | str' :: s' =>
                if String.isPrefix str str' then
                   p s
                else
                   raise SyntaxError)

      fun flex str p s =
         (case s of
             [] =>
                raise SyntaxError
           | str' :: s' =>
                if String.isPrefix str str' then
                   let
                      val str'' = String.extract (str', String.size str, NONE)
                   in
                      if str'' = "" then
                         p s'
                      else
                         p (str'' :: s')
                   end
                else
                   raise SyntaxError)

      fun scan pl s =
         let
            fun loop pl' s acc =
               (case pl' of
                   [] =>
                      (case s of
                          [] =>
                             ((), rev acc)
                        | str :: s' =>
                             loop pl s' (str :: acc))

                 | p :: rest =>
                      (let
                          val (x, s') = p s
                       in
                          loop pl s' acc
                       end
                       handle SyntaxError => loop rest s acc))
         in
            loop pl s []
         end
                      
      fun scanStrict pl s =
         let
            fun loop pl' s =
               (case pl' of
                   [] =>
                      ((), s)

                 | p :: rest =>
                      (let
                          val (x, s') = p s
                       in
                          loop pl s'
                       end
                       handle SyntaxError => loop rest s))
         in
            loop pl s
         end
                      
      fun parse p usage s =
         (p s; ())
         handle Exit => ()
              | Usage => print usage
              | SyntaxError => print usage

   end

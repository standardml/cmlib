
structure PrettyPrint
   :> PRETTY_PRINT
   =
   struct

      fun writeSpaces ios n =
         if n = 0 then
            ()
         else
            (
            TextIO.output1 (ios, #" ");
            writeSpaces ios (n-1)
            )

      datatype text =
         STRING of string
       | SPACES of int

      fun write ios text =
         (case text of
             STRING str =>
                TextIO.output (ios, str)
           | SPACES n =>
                writeSpaces ios n)

      fun size text =
         (case text of
             STRING str =>
                String.size str
           | SPACES n =>
                n)



      datatype box =
         Horizontal   (* all breaks horizontal *)
       | Vertical     (* all breaks vertical *)
       | Consistent   (* all breaks horizontal or all breaks vertical (prefer horizontal) *)
       | Freestyle    (* unrestricted (prefer horizontal breaks where possible) *)

      datatype action =
         Break of int
       | Text of text
       | Newline
       | OpenBox of box * int
       | CloseBox
       | Flush


      (* We want the formatter to run on-line, so that it can print its output eagerly when
         it's appropriate to do so.  Thus, we want to set up coroutines between the formatter
         and the user.  However, we want this code to work on implementations that don't support
         call/cc, so the formatter is implemented using explicit continuations.

         The send and receive functions implement a channel mechanism:  The user sends actions
         to the formatter by enqueueing them onto an imperative queue.  The imperative queue is
         converted into an infinite pollable stream, which the formatter polls for actions.
         When no action is available, the formatter "blocks" and returns a continuation that
         can be used to resume it.

         The formatter uses a pollable stream, rather than an imperative queue directly, because
         sometimes it needs to backtrack and re-process its input.
      *)


      (* Formatter (action consumer) *)

      datatype routine = R of unit -> routine   (* half a coroutine, so to speak *)

      datatype mode = T | H | V | F

      exception NotInBox
      exception Failure of exn * routine


      (* In what follows:

         env      = formatting environment = (iostream, width, indent, ck, mode)

         iostream = the device's output stream
         width    = the device's width
         indent   = current indentation
         ck       = continuation to close a box
         mode     = the current mode

         bk       = continuation to backtrack, if any
         input    = input channel
         output   = output queue, if buffering
         margin   = space remaining on current line

         sk       = success continuation (takes input, output, margin)

         We do output eagerly when there is no backtracking point, since there
         is no possibility then that a decision will need to be undone.

         Consequently, it is an invariant that output=[] whenever bk=NONE.
         To maintain this, we must flush the output queue whenever bk becomes
         NONE.

         Note that we always backtrack to the outermost decision point.  (It's not
         necessary to do so, it just looks better.)  Consequently, there's no need to
         remember older backtracking points.
      *)

      fun receive input f =
         (case PStream.poll input of
             NONE =>
                R (fn () => receive input f)
           | SOME PStream.Nil =>
                (* This stream never closes. *)
                raise (Fail "invariant")
           | SOME (PStream.Cons p) =>
                f p)

      fun enqueue iostream bk output text =
         (case bk of
             SOME _ =>
                text :: output
           | NONE =>
                (
                (* By invariant, output=[]. *)
                write iostream text;
                []
                ))

      fun flushOutput iostream output =
         app (write iostream) (rev output)

      fun ppNewline (env as (iostream, width, indent, _, _)) bk output k =
         let
            val output' =
               enqueue iostream bk
               (enqueue iostream bk output (STRING "\n")) 
               (SPACES indent)

            val margin' = width - indent
         in
            k output' margin' 
         end



      fun pp (env as (iostream, width, _, ck, mode)) bk input output margin =
         receive input
         (fn (action, input') =>
             (case action of
                 Break n =>
                    (case mode of
                        T =>
                           raise (Failure (NotInBox,
                                           R (fn () => pp env bk input' output margin)))
                      | H =>
                           (* Write spaces if there's room, or if there's no option to backtrack. *)
                           if n < margin orelse not (Option.isSome bk) then
                              let
                                 val output' = enqueue iostream bk output (SPACES n)
                                 val margin' = margin - n
                              in
                                 pp env bk input' output' margin' 
                              end
                           else
                              Option.valOf bk ()
                      | V =>
                           ppNewline env bk output (pp env bk input')
                      | F =>
                           (* Write spaces if there's room, but set a backtracking point that will expire
                              at the end of the line.
                           *)
                           if n < margin then
                              let
                                 (* Notionally we are expiring the backtracking point, thereby
                                    restoring NONE, before setting a new one.  Thus, we flush
                                    the output queue.
                                 *)
                                 val () = flushOutput iostream output

                                 fun k () =
                                    (* Restore the backtracking point to NONE. *)
                                    ppNewline env NONE [] (pp env NONE input')

                                 (* We know we have an backtracking point live, so we can enqueue directly. *)
                                 val output' = [SPACES n]
                                 val margin' = margin - n
                              in
                                 pp env (SOME k) input' output' margin' 
                              end
                           else
                              (
                              (* Expire the backtracking point. *)
                              flushOutput iostream output;
                              ppNewline env NONE [] (pp env NONE input')
                              ))
               | Newline =>
                    let
                       val (bk', output') =
                          (case mode of
                              F => 
                                 (* Expire the backtracking point. *)
                                 (
                                 flushOutput iostream output;
                                 (NONE, [])
                                 )
                            | _ =>
                                 (bk, output))
                    in
                       ppNewline env bk' output' (pp env bk' input')
                    end
               | Text text =>
                    let
                       val sz = size text
                    in
                       (* Write text if there's room, or if there's no option to backtrack. *)
                       if sz < margin orelse not (Option.isSome bk) then
                          let
                             val output' = enqueue iostream bk output text
                             val margin' = margin - sz
                          in
                             pp env bk input' output' margin'
                          end
                       else
                          (* Backtrack *)
                          Option.valOf bk ()
                    end
               | OpenBox p =>
                    ppOpenBox env bk input' output margin p (pp env bk)
               | CloseBox =>
                    ck input' output margin
               | Flush =>
                    (case mode of
                        T =>
                           pp env bk input' output margin
                      | _ =>
                           (* Leave the Flush action on the input stream. *)
                           ck input output margin)))

      and ppOpenBox (env as (iostream, width, indent, _, _)) bk input output margin (box, tab) sk =
         let
            val indent' = Int.max (width - margin + tab, 0)
         in
            (case box of
                Horizontal =>
                   (* Horizontal mode never sets a backtracking point, so return from Horizontal
                      can never unset one, so we won't need to flush the output queue.
                   *)
                   pp (iostream, width, indent', sk, H) bk input output margin
              | Vertical =>
                   (* Vertical mode never sets a backtracking point, so return from Vertical
                      can never unset one, so we won't need to flush the output queue.
                   *)
                   pp (iostream, width, indent', sk, V) bk input output margin
              | Consistent =>
                   let
                      val (ck', bk') =
                         (case bk of
                             SOME _ =>
                                (* If we already have a backtracking point, treat this as horizontal. *)
                                (sk, bk)
                           | NONE =>
                                let
                                   fun ck' input2 output2 margin2 =
                                      (* Returning resets the backtracking point to NONE, so flush the
                                         output queue.
                                      *)
                                      (
                                      flushOutput iostream output2;
                                      sk input2 [] margin2
                                      )

                                   fun k () =
                                      (* Never need to flush here: if bk=NONE then output=[] already. *)
                                      pp (iostream, width, indent', sk, V) NONE input output margin
                                in
                                   (ck', SOME k)
                                end)
                   in
                      pp (iostream, width, indent', ck', H) bk' input output margin 
                   end
              | Freestyle =>
                   (case bk of
                       SOME _ =>
                          (* If we already have a backtracking point, treat this as horizontal. *)
                          pp (iostream, width, indent', sk, H) bk input output margin
                     | NONE =>
                          let
                             fun ck' input2 output2 margin2 =
                                (* Returning resets the backtracking point to NONE, so flush the
                                   output queue.
                                *)
                                (
                                flushOutput iostream output2;
                                sk input2 [] margin2
                                )
                          in
                             pp (iostream, width, indent', ck', F) NONE input output margin
                          end))
            
         end

      fun ppStart iostream width input =
         let
            fun ck input2 output2 margin2 =
               (* Trying to close the outer box.  Complain. *)
               raise (Failure (NotInBox,
                               R (fn () =>
                                     pp (iostream, width, 0, ck, T) NONE input2 output2 margin2)))
         in
            pp (iostream, width, 0, ck, T) NONE input [] width
         end



      (* Action generator *)

      type ppstream = { queue : action IQueue.iqueue, formatter : routine ref }

      fun send ({ queue, formatter, ... }:ppstream) action =
         let
            val () = IQueue.insert queue action

            val R f = !formatter
         in
            formatter := f ()
            handle 
               Failure (ex, r) => 
                  (
                  formatter := r;
                  raise ex
                  )
         end

      fun makeStream iostream width =
         let
            val q = IQueue.iqueue ()
            val input = PStream.fromIqueue q
         in
            { queue = q, formatter = ref (R (fn () => ppStart iostream width input)) }
         end

      fun print st str = send st (Text (STRING str))

      fun tie st n = send st (Text (SPACES n))

      fun break st n = send st (Break n)

      fun newline st = send st Newline

      fun openBox st box tab = send st (OpenBox (box, tab))

      fun closeBox st = send st CloseBox

      fun flush st = send st Flush
      
   end


structure Path :> PATH =
   struct

      exception Path

      val explode = String.fields (fn ch => ch = #"/")

      val implode = String.concatWith "/"

      fun canonize pathstr =
         let
            fun loop acc path =
               (case path of
                   ".." :: rest =>
                      (case acc of
                          nil =>
                             loop (".." :: acc) rest

                        | ".." :: _ =>
                             loop (".." :: acc) rest

                        | "" :: _ =>
                             (* can't back up past the root *)
                             raise Path

                        | _ :: restacc =>
                             loop restacc rest)

                 | [] => rev acc
                   
                 | [""] =>
                      (* trailing slash *)
                      rev acc

                 | "." :: _ => 
                      (* can't have . in the middle of a path *)
                      raise Path

                 | "" :: _ =>
                      (* can't have an empty directory name *)
                      raise Path

                 | name :: rest =>
                      loop (name :: acc) rest)
         in
            (case explode pathstr of
                [] => raise Path
                
              | ["."] => "."
              | [".", ""] => "."

              | "." :: rest => implode (loop [] rest)

              | "" :: rest =>
                   (* absolute path *)
                   implode (loop [""] rest)

              | path => implode (loop [] path))
         end

      fun isAbsolute path =
         String.sub (path, 0) = #"/"
         handle Subscript => raise Path

      fun isRelative path =
         String.sub (path, 0) <> #"/"
         handle Subscript => raise Path

      fun join path1 path2 =
         String.concat [path1, "/", path2]

      fun makeAbsolute path1 path2 =
         if isAbsolute path2 then
            path2
         else
            join path1 path2

      fun split path =
         let
            fun loop acc l =
               (case l of
                   nil => raise Path

                 | [""] =>
                      (* filename is empty *)
                      raise Path

                 | [filename] =>
                      (case acc of
                          [] => (".", filename)

                        | [""] => ("/", filename)

                        | _ => (implode (rev acc), filename))

                 | h :: t =>
                      loop (h :: acc) t)
         in
            loop [] (explode path)
         end

      fun joinExt path1 path2 =
         String.concat [path1, ".", path2]

      fun splitExt path =
         let
            fun loop i =
               if i < 0 then
                  NONE
               else if String.sub (path, i) = #"." then
                  SOME (String.substring (path, 0, i), String.extract (path, i+1, NONE))
               else
                  loop (i-1)
         in
            loop (String.size path - 1)
         end

   end

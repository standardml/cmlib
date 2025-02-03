
structure Path :> PATH =
   struct

      exception Path

      val explode = String.fields (fn ch => ch = #"/")

      val implode = String.concatWith "/"


      fun canonizeLoop acc path =
         (case path of
             ".." :: rest =>
                (case acc of
                    nil =>
                       canonizeLoop (".." :: acc) rest

                  | ".." :: _ =>
                       canonizeLoop (".." :: acc) rest

                  | "" :: _ =>
                       (* can't back up past the root *)
                       raise Path

                  | _ :: restacc =>
                       canonizeLoop restacc rest)

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
                canonizeLoop (name :: acc) rest)

      fun canonizeArcs path =
         (case path of
             [] => raise Path
             
           | ["."] => ["."]
           | [".", ""] => ["."]

           | "." :: rest => canonizeLoop [] rest

           | ["", ""] =>
                (* root *)
                ["", ""]

           | "" :: rest =>
                (* absolute path *)
                canonizeLoop [""] rest

           | path => canonizeLoop [] path)

      fun canonize pathstr = implode (canonizeArcs (explode pathstr))


      fun isAbsolute path =
         String.sub (path, 0) = #"/"
         handle Subscript => raise Path

      fun isRelative path =
         String.sub (path, 0) <> #"/"
         handle Subscript => raise Path

      fun hasPath name =
         let
            val len = String.size name
            
            fun loop i =
               if i = len then
                  false
               else if String.sub (name, i) = #"/" then
                  true
               else
                  loop (i+1)
         in
            loop 0
         end

      fun join path1 path2 =
         if isAbsolute path2 then
            raise Path
         else if 
            String.size path1 >= 1 
            andalso
            String.sub (path1, String.size path1 - 1) = #"/"
         then
            path1 ^ path2
         else
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



      val illegalOnWindows = Char.contains "\\/:*?\"<>|"

      fun illegalWindowsLoop str i len =
         if i >= len then
            false
         else if illegalOnWindows (String.sub (str, i)) then
            true
         else
            illegalWindowsLoop str (i+1) len

      fun illegalWindows str = illegalWindowsLoop str 0 (String.size str)


      fun volumeId str =
         if
            String.size str = 2
            andalso
            String.sub (str, 1) = #":"
         then
            let
               val ch = String.sub (str, 0)
            in
               if Char.isAlpha ch then
                  SOME (Char.toLower ch)
               else
                  NONE
            end
         else
            NONE
         

      fun toWindowsPath pathstr =
         let
            val path = canonizeArcs (explode pathstr)
         in
            (case path of
                [] =>
                   (* Empty path.  Canonization won't allow this, but we'll just handle it. *)
                   raise Path

              | [""] =>
                   (* Empty filename.  Canonization won't allow this, but we'll just handle it. *)
                   raise Path

              | "" :: first :: rest =>
                   (* prospective absolute path, check volume id *)
                   (case volumeId first of
                       SOME ch =>
                          if List.exists illegalWindows rest then
                             raise Path
                          else
                             String.concatWith "\\"
                                (String.str ch ^ ":"
                                 ::
                                 (case rest of
                                     nil => [""]
                                   | _ => rest))

                     | NONE =>
                          (* absolute path without volume id *)
                          raise Path)

              | _ =>
                   (* relative or simple path *)
                   if List.exists illegalWindows path then
                      raise Path
                   else
                      String.concatWith "\\" path)
         end

      fun fromWindowsPath pathstr =
         let
            val path = canonizeArcs (String.fields (fn ch => ch = #"\\") pathstr)
         in
            (case path of
                [] =>
                   (* Empty path.  Canonization won't allow this, but we'll just handle it. *)
                   raise Path

              | [""] =>
                   (* Empty filename.  Canonization won't allow this, but we'll just handle it. *)
                   raise Path

              | "" :: _ =>
                   (* Absolute path without volume id. *)
                   raise Path

              | first :: rest =>
                   (case volumeId first of
                       SOME ch =>
                          implode ("" :: String.str ch ^ ":" :: rest)

                     | NONE =>
                          (* relative or simple path *)
                          implode path))
         end


      fun fromHybridPath pathstr =
         let
            val path = canonizeArcs (String.fields (fn ch => ch = #"/" orelse ch = #"\\") pathstr)
         in
            (case path of
                [] =>
                   (* Empty path.  Canonization won't allow this, but we'll just handle it. *)
                   raise Path

              | [""] =>
                   (* Empty filename.  Canonization won't allow this, but we'll just handle it. *)
                   raise Path

              | "" :: _ =>
                   (* Absolute path without volume id. *)
                   implode path

              | first :: rest =>
                   (case volumeId first of
                       SOME ch =>
                          implode ("" :: String.str ch ^ ":" :: rest)

                     | NONE =>
                          (* relative or simple path *)
                          implode path))
         end

   end

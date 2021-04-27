
(* Manipulation of Unix paths.  A substitute for OS.Path, which is over-general,
   and whose implementation varies between platforms.  This library works on Unix
   paths only, whatever platform you happen to be working on.
*)

signature PATH =
   sig
   
      (* argument is not a good path *)
      exception Path

      val explode : string -> string list
      val implode : string list -> string

      (* A canonical path:
         (1) is nonempty
         (2) contains . only as the entire path
         (3) contains .. only at the beginning
         (4) contains no empty directories
         (5) filename is not empty (ie, no trailing /)

         Raises Path if the path cannot be canonized.
      *)
      val canonize : string -> string

      val isAbsolute : string -> bool
      val isRelative : string -> bool
      val hasPath : string -> bool

      (* makeAbsolute path1 path2
       
         if    path1 and path2 are canonical, and path1 is absolute
         then  returns an absolute path corresponding to path2, using path1 as the
               current directory
      *)
      val makeAbsolute : string -> string -> string

      (* joins a path to either a filename or a relative path *)
      val join : string -> string -> string

      (* separate the path from the filename *)
      val split : string -> string * string

      (* join a filename to an extension (filename can be a path) *)
      val joinExt : string -> string -> string

      (* separate the filename from its extension (filename can be a path) *)
      val splitExt : string -> (string * string) option

   end

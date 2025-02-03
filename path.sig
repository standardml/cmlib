
(* Manipulation of Unix paths.  A substitute for OS.Path, which is over-general,
   and whose implementation varies between platforms.  This library operates on
   Unix paths, whatever platform you happen to be working on.
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


      (* The absolute Windows path c:\a\b is represented by the Unix path /c:/a/b

         (Apart from tweaking the notation for the volume id, there doesn't seem to be
         a better stateless way to represent Windows paths as Unix paths.)

         Raises Path if the path is unrepresentable on the destination platform:

         - An absolute Windows path without volume id is unrepresentable on Unix
           (since it is not a relative path nor a fully absolute path).

         - An absolute Unix path that does not begin with a volume id is not
           representable on Windows.

         - A Unix path using forbidden characters (that is, \ / : * ? " < > | )
           is unrepresentable on Windows.

         The behavior of toWindowsPath on illegal Unix paths and of fromWindowsPath
         on illegal Windows paths is undefined.
      *)
      val toWindowsPath : string -> string
      val fromWindowsPath : string -> string


      (* A hybrid path can use either separator (slash or backslash).  Backslash
         cannot be used except as a separator, even though Unix paths permit it.  The
         path may begin with a volume id or not; if it does, it is interpreted as
         in fromWindowsPath.

         Hybrid paths are designed to accept valid Unix paths (provided they do not
         use backslash, nor use colon in a way that looks like a volume id) and also
         valid Windows paths (provided they include a volume id if they are absolute).
      *)
      val fromHybridPath : string -> string

   end

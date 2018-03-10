
(* A hybrid between functional and imperative dictionaries.  Insert/find/lookup operate
   on a frame dictionary as if it were functional.  AddFrame adds an imperative "frame"
   to which elements can be inserted imperatively.
*)

signature FRAME_DICT =
   sig

      type init
      type key
      type 'a dict
      type 'a frame

      exception Absent

      val create : init -> 'a frame * 'a dict

      val insert : 'a dict -> key -> 'a -> 'a dict
      val find : 'a dict -> key -> 'a option
      val lookup : 'a dict -> key -> 'a

      val addFrame : 'a dict -> 'a frame * 'a dict
      val memberFrame : 'a frame -> key -> bool
      val findFrame : 'a frame -> key -> 'a option
      val insertFrame : 'a frame -> key -> 'a -> unit
      val foldFrame : (key * 'a * 'b -> 'b) -> 'b -> 'a frame -> 'b

   end

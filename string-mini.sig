
signature MINI_STRING =
   sig

      type string
      type elem
      
      val null : string
      val sub : string * int -> elem
      val size : string -> int
      val extract : string * int * int option -> string
      val concat : string list -> string
      val implode : elem list -> string
      val explode : string -> elem list

   end

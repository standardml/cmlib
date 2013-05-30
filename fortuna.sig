
(* The module does not provide automatic collection of entropy.  The application
   does so, and then injects it using addEntropy.  The module also does not 
   automatically maintain a seed file.  The application may do so, using initialSeed
   to import the seed file's contents, and using random to generate seed file contents.
   It might also be useful to provide a facility to save the entropy.
*)

signature FORTUNA =
   sig

      include RANDOM where type seed = Bytestring.string

      val poolCount : int

      (* takes (pool number, entropy) *)
      val addEntropy : int * Bytestring.string -> unit

      val initialSeed : Bytestring.string -> unit

   end

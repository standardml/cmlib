
signature HASHABLE_SET =
   sig

      include SET

      structure Hashable : HASHABLE where type t = set

   end

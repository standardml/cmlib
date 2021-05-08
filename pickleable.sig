
signature PICKLEABLE_TYPE =
   sig
      type t

      val pu : t Pickle.pu
   end


signature PICKLEABLE_CON =
   sig
      type 'a t

      val pu : 'a Pickle.pu -> 'a t Pickle.pu
   end

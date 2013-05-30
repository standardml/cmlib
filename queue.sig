
signature QUEUE =
   sig
      type 'a queue

      exception Empty
      val empty : 'a queue
      val insert : 'a queue -> 'a -> 'a queue
      val isEmpty : 'a queue -> bool
      val front : 'a queue -> 'a * 'a queue
   end


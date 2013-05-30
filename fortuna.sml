
functor FortunaFun (structure Random : RANDOM where type seed = Bytestring.string)
   :>
   FORTUNA
   =
   struct

      val poolCount = 32

      val minReseedTime : LargeInt.int = 100 (* milliseconds *)

      (* Require 256 bits of entropy to reseed, and assume each event provides 4 bits. *)
      val minPoolEvents = 64


      val null : Word8.word Stream.stream = Stream.eager Stream.Nil

      
      val pools = Array.array (poolCount, (SHA256.initial, 0))  (* each pool contains (state, # of events) *)
      val lastReseed : LargeInt.int ref = ref 0
      val reseedCycle : LargeInt.int ref = ref 1

      fun addEntropy (pool, str) =
         if pool < 0 orelse pool >= poolCount then
            raise Domain
         else
            let
               val (st, events) = Array.sub (pools, pool)
            in
               Array.update (pools, pool, (SHA256.update (st, str), events+1))
            end


      fun tryReseed () =
         let
            val time = Time.toMilliseconds (Time.now ())
         in
            if time >= !lastReseed + minReseedTime then
               let
                  val cycle = !reseedCycle
                  
                  (* p = 2^i *)
                  fun loop acc i p =
                     if i >= poolCount then
                        SHA256.finish (acc, null)
                     else
                        if cycle mod p = 0 then
                           let
                              val (st, events) = Array.sub (pools, i)
                           in
                              if events >= minPoolEvents then
                                 let
                                    val str = SHA256.finish (st, null)
                                 in
                                    Array.update (pools, i, (SHA256.initial, 0));
                                    loop (SHA256.update (acc, str)) (i+1) (p*2)
                                 end
                              else
                                 loop acc (i+1) (p*2)
                           end
                        else
                           loop acc (i+1) (p*2)

                  val entropy = loop SHA256.initial 0 1
               in
                  lastReseed := time;
                  reseedCycle := cycle + 1;
                  Random.reseed entropy
               end
            else
               ()
         end

   
      fun random n =
         (
         if #2 (Array.sub (pools, 0)) >= minPoolEvents then
            tryReseed ()
         else
            ();
         Random.random n
         )


      fun initialSeed seed = Random.reseed seed


      type seed = Bytestring.string

      (* We'll treat a reseed as as addEntropy to pool 0. *)
      fun reseed str = addEntropy (0, str)

   end

structure AESFortuna = FortunaFun (structure Random = AESRandom)

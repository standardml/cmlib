(* Adapted from Chris Okasaki, Robert Harper, and Tom Murphy VII's position
 * library. *)

structure Coord :> COORD = 
   struct
      type coord = {file: string, char: int, line: int, abs: int}
      type t = coord

      fun init s = {file = s, char = 1, line = 1, abs = 1}

      fun nextchar {file, char, line, abs} = 
         {file = file, char = char + 1, line = line, abs = abs + 1}
   
      fun nextline {file, char, line, abs} = 
         {file = file, char = 1, line = line + 1, abs = abs + 1}

      fun addchar n {file, char, line, abs} = 
         {file = file, char = char + n, line = line, abs = abs + n}

      fun file (pos: coord) = #file pos

      fun abs (pos: coord) = #abs pos
      
      fun line (pos: coord) = #line pos
  
      fun char (pos: coord) = #char pos

      fun eq (pos1, pos2) = abs pos1 = abs pos2

      fun compare (pos1, pos2) = Int.compare (abs pos1, abs pos2)
 
      fun hash pos = Word.fromInt (abs pos)

      fun toString {file, char, line, abs} = 
         file ^ ":" ^ Int.toString line ^  "." ^ Int.toString char

      fun leftmost pos1 pos2 = 
         case compare (pos1, pos2) of
            GREATER => pos2
          | _ => pos1

      fun rightmost pos1 pos2 =
         case compare (pos1, pos2) of
            LESS => pos2
          | _ => pos1
   end

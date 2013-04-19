(*
WLOG, pivot is the first element of given list in the following implementation.
*)
signature SORT =
sig
    val quicksort : int list -> int list
end
    
structure quicksort:>SORT =
struct


fun quicksort lst =
    case lst of
	[]=> []
      | hd::[] => [hd]
      | pivot::rest => 
	let
	    val leftright = List.partition (fn x => x < pivot) rest
	    (*partition : ('a -> bool) -> 'a list -> 'a list * 'a list*)
	    val left = #1 leftright
	    val right = #2 leftright
	in
	    quicksort left @ [pivot] @ quicksort right
						 
	end			       
	    
end

(*test cases 
(*Check how partition works
val tup=List.partition (fn x => x > 1) [1,2,1,2,4,~2,3,~5];
val left = #1 tup
val right = #2 tup
*)

val sort1 = quicksort[1,2,1,2,4,~2,3,~5] = [~5,~2,1,1,2,2,3,4];
val sort2 = quicksort [4,5,1,2,0,100,200,6,7,3]= [0,1,2,3,4,5,6,7,100,200];
*)

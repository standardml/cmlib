
structure Quicksort :> SORT =
struct


fun compare f x y =
    case f (x, y) of
	LESS => true
      | _ =>  false
		  
		  
fun sort f lst =
    case lst of
	[]=> []
      | hd::[] => [hd]
      | pivot::rest => 
	let
	    val (left, right) = List.partition (fn x => compare f x pivot) rest
	            (*partition : ('a -> bool) -> 'a list -> 'a list * 'a list*)
	in
	    sort f left @ [pivot] @ sort f right
					 
	end			       
end    

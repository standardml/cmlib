(*
Bubble sort.
*)
signature SORT =
sig
    val bubblesort : int list -> int list
    val sweep : int list -> int list
end
    
structure bubblesort:>SORT =
struct

fun sweep lst = (*int list -> int list*)
(* Goes through the list and swaps adjacent items if need be.*)
    case lst of
	xs::ys::zs=> if xs > ys then ys :: sweep (xs::zs)
		  else xs :: sweep (ys::zs)
      | xs::xs' => xs :: sweep(xs')
      | [] => []

fun bubblesort lst = (* int list -> int list*)
(*sorts the whole list*)
    let
	val lst' = sweep lst
    in
	if lst'=lst then lst
	else bubblesort lst'
    end
end		  
		  
(* test cases*)
(*
val sort1 = bubblesort.bubblesort [3,2,5,1,7,2] = [1,2,2,3,5,7];
val sort2 = bubblesort.bubblesort [2,3,5,4,6,7,1,8,9,0]= [0,1,2,3,4,5,6,7,8,9];
val sort3 = bubblesort.bubblesort [] = [];
val sort4 = bubblesort.bubblesort [1] = [1];
*)

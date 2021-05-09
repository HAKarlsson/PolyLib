(*
Sort.sort Int.compare [4,3,5,1,2]
*)
structure ListUtil =
struct
fun sort ord lst = 
    let
	fun split [] = ([],[])
	  | split [x] = ([x], [])
	  | split (x::y::lst) = 
	    let
		val (xs, ys) = split lst
	    in
		(x::xs, y::ys)
	    end
	fun merge ([],[]) = []
	  | merge (xs,[]) = xs
	  | merge ([],ys) = ys
	  | merge (x::xs,y::ys) = 
	    case ord (x,y)
	     of LESS => x::(merge (xs,y::ys))
	      | _ => y::(merge (x::xs, ys))
	fun sort' [] = []
	  | sort' [x] = [x]
	  | sort' lst =
	    let
		val (xs, ys) = split lst
	    in
		merge (sort' xs, sort' ys)
	    end
    in
	sort' lst
    end
end

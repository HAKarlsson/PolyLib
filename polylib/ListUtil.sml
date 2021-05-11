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

fun intersperse _ [] = []
  | intersperse sep (x::xs) =
    let
	fun f [] = []
	  | f (x::xs) = sep::x::f xs
    in
	x::f xs
    end

fun intercalate xs xss = List.concat (intersperse xs xss)

fun span _ [] = ([],[])
  | span p (x::xs) = 
    if p x
    then let val (ys, zs) = span p xs in (x::ys, zs) end
    else ([], x::xs)
	   
fun break p = span (not o p)

fun takeWhile _ [] = []
  | takeWhile p (x::xs) = 
    if p x
    then x::takeWhile p xs
    else []

fun dropWhile _ [] = []
  | dropWhile p (x::xs) = 
    if p x
    then dropWhile p xs
    else (x::xs) 

fun split p xs =
    let val (ys, zs) = break p xs in
	(ys, dropWhile p zs)
    end

fun groupBy _ [] = []
  | groupBy eq (x::xs) =
    let
	val (ys, zs) = span (fn y => eq(x,y)) xs
    in
	(x::ys)::groupBy eq zs
    end

fun deleteBy _ _ [] = []
  | deleteBy eq x (y::ys) = if eq(x, y) then ys else y::deleteBy eq x ys 

fun nubBy _ [] = []
  | nubBy eq (x::xs) = x::(nubBy eq (List.filter (fn y => not(eq(x, y))) xs))

fun unionBy eq xs ys = xs @ foldl (fn (x,ys) => deleteBy eq x ys) ys xs

fun intersectBy eq _ [] = []
  | intersectBy eq [] _ = []
  | intersectBy eq xs ys = List.filter (fn x => List.exists (fn y => eq(x,y)) ys) xs 

fun group xs = groupBy (op=) xs
fun delete x ys = deleteBy (op=) x ys
fun nub xs = nubBy (op=) xs
fun union xs ys = unionBy (op=) xs ys
fun intersect xs ys = intersectBy (op=) xs ys

fun fLookupBy eq k [] = NONE
  | fLookupBy eq k ((k',v')::xs) = 
    if eq(k,k')
    then SOME v'
    else fLookupBy eq k xs

fun fDeleteBy _ _ [] = []
  | fDeleteBy eq k ((k',v')::xs) = 
    if eq(k,k')
    then xs
    else (k',v')::fDeleteBy eq k xs

fun fUpdateBy _ x [] = [x]
  | fUpdateBy eq (k,v) ((k',v')::xs) =
    if eq(k,k') 
    then (k,v)::xs
    else (k',v')::fUpdateBy eq (k,v) xs


fun fLookup k xs = fLookupBy (op=) k xs
fun fDelete k xs = fDeleteBy (op=) k xs
fun fUpdate x xs = fUpdateBy (op=) x xs 

end

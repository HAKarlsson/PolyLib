(*
ListUtil.sort Int.compare [4,3,5,1,2,2,1]
*)
structure ListUtil =
struct

(* Merge sort *)
fun sort ord lst =
    let
	fun runUp ([], acc) = ([], List.rev acc)
	  | runUp ([x],acc) = ([], List.rev (x::acc))
	  | runUp (x::y::xs,acc) =
	    (case ord(x,y)
	      of GREATER => (y::xs, List.rev(x::acc))
	       | _ => runUp (y::xs,x::acc))
	fun runDown ([], acc) = ([], acc)
	  | runDown ([x],acc) = ([], x::acc)
	  | runDown (x::y::xs,acc) =
	    (case ord(x,y)
	      of LESS => (y::xs, x::acc)
	      |  _ => runDown (y::xs,x::acc))
	fun split ([], acc) = acc
	  | split ([x], acc) = [x]::acc
	  | split (x::y::xs,acc) = 
	    case ord(x,y)
	     of GREATER =>
		let val (rest, down) = runDown (y::xs, [x])
		in split (rest, down::acc) end
	      | _ =>
		let val (rest, up) = runUp (y::xs, [x])
		in split (rest, up::acc) end
	fun merge ([], ys, acc) = List.revAppend (acc, ys)
	  | merge (xs, [], acc) = List.revAppend (acc, xs) 
	  | merge (x::xs, y::ys, acc) = 
	    case ord(x,y)
	     of LESS => merge(xs, y::ys, x::acc)
	      | _ => merge(x::xs, ys, y::acc)
    in
	foldl (fn (xs,ys) => merge (xs,ys,[])) [] (split (lst,[]))
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


(* finite_map list *)
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


fun fLookup k xs = fLookupBy (op=) k xs;
fun fDelete k xs = fDeleteBy (op=) k xs;
fun fUpdate x xs = fUpdateBy (op=) x xs;

fun cartesianProd [] = []
  | cartesianProd [xs] = map (fn x => [x]) xs
  | cartesianProd (xs::xss) =
    let val yss = cartesianProd xss
    in List.concat (map (fn x => map (fn ys => x::ys) yss) xs) end;

end

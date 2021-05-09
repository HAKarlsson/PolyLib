functor AssocList(eqtype key) : MAP =
struct
type key = key
type 'a entry = key * 'a
type 'a map = ('a entry) list

val empty = [] : 'a map

fun prune [] = []
  | prune ((key,value)::lst) =
    let
	val filtered = List.filter (fn (key',_) => not(key = key')) lst
    in
	(key,value)::prune filtered
    end
					       

fun keys lst = map (fn (k,v) => k) (prune lst)
fun values lst = map (fn (k,v) => v) (prune lst)
val entries = prune
fun size lst = List.length (prune lst)
		
fun member lst key =
    let
	fun member' [] = false
	  | member' ((key',value')::lst) = 
	    if key = key'
	    then true
	    else member' lst
    in
	member' lst
    end

fun lookup lst key =
    let
	fun lookup' [] = NONE
	  | lookup' ((key',value')::lst) = 
	    if key = key'
	    then SOME(value')
	    else lookup' lst
    in
	lookup' lst
    end

fun insert (entry,lst) = entry::lst

end

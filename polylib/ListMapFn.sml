functor ListMapFn(K:ORD_KEY) : ORD_MAP =
struct

structure K = K

type 'a map = (K.key * 'a) list

val empty = [] : 'a map

fun isEmpty [] = true
  | isEmpty _ = false

fun keys lst = map (fn (k,v) => k) (lst)

fun values lst = map (fn (k,v) => v) (lst)

fun entries lst = lst

fun size lst = List.length lst
		
fun member lst key =
    let
	fun member' [] = false
	  | member' ((key',_)::lst) = 
	    case K.compare (key, key')
	     of LESS => false
	      | EQUAL => true
	      | GREATER => member' lst
    in
	member' lst
    end

fun lookup lst key =
    let
	fun lookup' [] = NONE
	  | lookup' ((key',value')::lst) = 
	    case K.compare (key, key')
	     of LESS => NONE
	      | EQUAL => SOME value'
	      | GREATER => lookup' lst
    in
	lookup' lst
    end

fun insert ((key,value),lst) =
    let
	fun insert' [] = [(key,value)]
	  | insert' ((key',value')::lst) =
	    case K.compare(key, key')
	     of LESS => (key,value)::(key',value')::lst
	      | EQUAL => (key,value)::lst
	      | GREATER => (key',value')::insert' lst
    in
	insert' lst
    end
end

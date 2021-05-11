functor TreeMapFn (K:ORD_KEY) : ORD_MAP =
struct

exception Impossible (* Internal error *)

structure K = K

datatype color = RED | BLACK
datatype 'a tree = LEAF
		 | BRANCH of color * 'a tree * (K.key * 'a) * 'a tree

type 'a map = 'a tree


val empty = LEAF

fun isEmpty LEAF = true
  | isEmpty (BRANCH _) = false

fun size LEAF = 0
  | size (BRANCH (_, left, (key, _), right)) = 1 + (size left) + (size right)

fun keys LEAF = []
  | keys (BRANCH (_, left, (key, _), right)) = (keys left) @ [key] @ (keys right)

fun values LEAF = []
  | values (BRANCH (_, left, (_, value), right)) = (values left) @ [value] @ (values right)

fun entries LEAF = []
  | entries (BRANCH (_, left, entry, right)) = (entries left) @ [entry] @ (entries right)

fun member root key =     
    let
	fun member' LEAF = false
	  | member' (BRANCH(_,left,(key',_),right)) = 
	    case K.compare(key,key')
	     of LESS => member' left
	      | EQUAL => true
	      | GREATER => member' right
    in
	member' root
    end

fun lookup root key =
    let
	fun lookup' LEAF = NONE
	  | lookup' (BRANCH(_,left,(key',value'),right)) = 
	    case K.compare(key,key')
	     of LESS => lookup' left
	      | EQUAL => SOME value'
	      | GREATER => lookup' right 
    in
	lookup' root
    end

local
    fun balance (BLACK,BRANCH(RED,BRANCH(RED,a,x,b),y,c),z,d) = BRANCH(RED,BRANCH(BLACK,a,x,b),y,BRANCH(BLACK,c,z,d))
      | balance (BLACK,BRANCH(RED,a,x,BRANCH(RED,b,y,c)),z,d) = BRANCH(RED,BRANCH(BLACK,a,x,b),y,BRANCH(BLACK,c,z,d))
      | balance (BLACK,a,x,BRANCH(RED,BRANCH(RED,b,y,c),z,d)) = BRANCH(RED,BRANCH(BLACK,a,x,b),y,BRANCH(BLACK,c,z,d))
      | balance (BLACK,a,x,BRANCH(RED,b,y,BRANCH(RED,c,z,d))) = BRANCH(RED,BRANCH(BLACK,a,x,b),y,BRANCH(BLACK,c,z,d))
      | balance t = BRANCH t
    fun makeBlack (BRANCH(_,left,entry,right)) = BRANCH(BLACK,left,entry,right)
      | makeBlack LEAF = raise Impossible
in
fun insert ((key,value), root) =
    let
	fun insert' LEAF = BRANCH(RED,LEAF,(key,value),LEAF)
	  | insert' (BRANCH(color,left,(key',value'),right)) = 
	    case K.compare(key,key')
	     of LESS => balance(color,insert' left,(key',value'),right)
	      | EQUAL => BRANCH(color,left,(key,value),right)
	      | GREATER => balance(color,left,(key',value'),insert' right)
    in
	makeBlack (insert' root)
    end
end
end

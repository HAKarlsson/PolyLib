signature ORD_KEY =
sig
    type key
    val compare : key * key -> order
end

functor TreeMapFn (K:ORD_KEY) =
struct
type key = K.key
val compare = K.compare

datatype color = R | B
datatype 'a tree = L
		 | T of color * 'a tree * (key * 'a) * 'a tree

val empty = L

fun member (k,r) =     
    let
	fun member' L = false
	  | member' (T(_,l,x,r)) = 
	    case compare(k,#1 x)
	     of LESS => member' l
	      | EQUAL => true
	      | GREATER => member' r 
    in
	member' r
    end

fun lookup (k,r) =
    let
	fun lookup' L = NONE
	  | lookup' (T(_,l,x,r)) = 
	    case compare(k, #1 x)
	     of LESS => lookup' l
	      | EQUAL => SOME (#2 x)
	      | GREATER => lookup' r 
    in
	lookup' r
    end

local
    fun balance (B,T(R,T(R,a,x,b),y,c),z,d) = T(R,T(B,a,x,b),y,T(B,c,z,d))
      | balance (B,T(R,a,x,T(R,b,y,c)),z,d) = T(R,T(B,a,x,b),y,T(B,c,z,d))
      | balance (B,a,x,T(R,T(R,b,y,c),z,d)) = T(R,T(B,a,x,b),y,T(B,c,z,d))
      | balance (B,a,x,T(R,b,y,T(R,c,z,d))) = T(R,T(B,a,x,b),y,T(B,c,z,d))
      | balance b = T b
in
fun insert (x,r) =
    let
	fun insert' L = T(R,L,x,L)
	  | insert' (T(c,a,y,b)) = 
	    case compare(#1 x,#1 y)
	     of LESS => balance(c,insert' a,y,b)
	      | EQUAL => T(c,a,y,b)
	      | GREATER => balance(c,a,y,insert' b)
	fun makeBlack L = L
	  | makeBlack (T(_,a,x,b)) = T(B,a,x,b)
    in
	makeBlack (insert' r)
    end
end
end

(* Example usage:
 * structure S = TreeMapFn(type key=int; val compare=Int.compare) 
 *)

(* Set based on RedBlackTree *)
functor SetFn (type key; val compare: key * key -> order) : SET =
struct

type key = key 

exception Impossible (* Internal error *)

datatype color = R | B
datatype set
  = LEAF
  | BRAN of color * set * key * set

val empty = LEAF

fun member (root, b) =
    let
	fun loop LEAF = false
	  | loop (BRAN(_,t1,a,t2)) = 
	    case compare(a,b)
	     of GREATER => loop t1
	      | EQUAL => true
	      | LESS => loop  t2
    in
	loop root
    end

fun balance (B,BRAN(R,BRAN(R,a,x,b),y,c),z,d) = BRAN(R,BRAN(B,a,x,b),y,BRAN(B,c,z,d))
  | balance (B,BRAN(R,a,x,BRAN(R,b,y,c)),z,d) = BRAN(R,BRAN(B,a,x,b),y,BRAN(B,c,z,d))
  | balance (B,a,x,BRAN(R,BRAN(R,b,y,c),z,d)) = BRAN(R,BRAN(B,a,x,b),y,BRAN(B,c,z,d))
  | balance (B,a,x,BRAN(R,b,y,BRAN(R,c,z,d))) = BRAN(R,BRAN(B,a,x,b),y,BRAN(B,c,z,d))
  | balance t = BRAN t

fun makeBlack (BRAN(_,left,entry,right)) = BRAN(B,left,entry,right)
  | makeBlack LEAF = raise Impossible

fun update (root, b) =
    let
	fun loop LEAF = BRAN(R,LEAF,b,LEAF)
	  | loop (BRAN(c,t1,a,t2)) = 
	    case compare(a,b)
	     of GREATER => balance(c,loop t1,a,t2)
	      | EQUAL => BRAN(c,t1,b,t2)
	      | LESS => balance(c,t1,a,loop t2)
    in
	makeBlack (loop root)
    end

fun make lst =
    let
	fun loop ([],r) = r
	  | loop (a::lst, r) = loop (lst, update (r, a))
    in
	loop (lst, LEAF)
    end
end
(*
structure S = DictionaryFn(type key = int; val compare = Int.compare)
val s = S.update (S.empty, 1, "hello")
*)

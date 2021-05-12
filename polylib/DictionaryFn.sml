functor DictionaryFn (Key:ORDER) : DICTIONARY =
struct

type key = Key.t
exception Impossible (* Internal error *)
exception E of key
datatype color = R | B
datatype 'a t = LEAF
	      | BRAN of color * 'a t * (key * 'a) * 'a t

val empty = LEAF

fun lookup (root, b) =
    let
	fun loop LEAF = NONE
	  | loop (BRAN(_,t1,(a,x),t2)) = 
	    case Key.compare(a,b)
	     of GREATER => loop t1
	      | EQUAL => SOME x 
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

fun insert (root, b, y) =
    let
	fun loop LEAF = BRAN(R,LEAF,(b, y),LEAF)
	  | loop (BRAN(c,t1,(a,x),t2)) = 
	    case Key.compare(a,b)
	     of GREATER => balance(c,loop t1,(a,x),t2)
	      | EQUAL => raise E b
	      | LESS => balance(c,t1,(a,x),loop t2)
    in
	makeBlack (loop root)
    end

fun update (root, b, y) =
    let
	fun loop LEAF = BRAN(R,LEAF,(b, y),LEAF)
	  | loop (BRAN(c,t1,(a,x),t2)) = 
	    case Key.compare(a,b)
	     of GREATER => balance(c,loop t1,(a,x),t2)
	      | EQUAL => BRAN(c,t1,(b,y),t2)
	      | LESS => balance(c,t1,(a,x),loop t2)
    in
	makeBlack (loop root)
    end

fun make lst =
    let
	fun loop ([],r) = r
	  | loop ((a,x)::lst, r) = loop (lst, update (r, a, x))
    in
	loop (lst, LEAF)
    end
end
(*
structure S = DictionaryFn(type t = int; val compare = Int.compare)
val s = S.update (S.empty, 1, "hello")
*)

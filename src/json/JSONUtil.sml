signature JSONUtil =
sig
    exception NotBool of JSON.value
    exception NotInt of JSON.value
    exception NotNumber of JSON.value
    exception NotString of JSON.value
    exception NotObject of JSON.value
    exception FieldNotFound of JSON.value * string
    exception NotArray of JSON.value
    exception ArrayBounds of JSON.value * int
    exception ElemNotFound of JSON.value
    val exnMessage : exn -> string
    val asBool : JSON.value -> bool
    val asInt : JSON.value -> Int.int
    val asIntInf : JSON.value -> IntInf.int
    val asNumber : JSON.value -> Real.real
    val asString : JSON.value -> string
    val findField : JSON.value -> string -> JSON.value option
    val lookupField : JSON.value -> string -> JSON.value
    val hasField : string -> JSON.value -> bool
    val testField : string -> (JSON.value -> bool) -> JSON.value -> bool
    val asArray : JSON.value -> JSON.value vector
    val arrayMap : (JSON.value -> 'a) -> JSON.value -> 'a list
    datatype edge
      = SEL of string
      | SUB of int
      | FIND of JSON.value -> bool
    type path = edge list
    val get : JSON.value * path -> JSON.value
end

structure JSONUtil :> JSONUtil =
struct
open JSON;
exception NotBool of value
exception NotInt of value
exception NotNumber of value
exception NotString of value
exception NotObject of value
exception FieldNotFound of value * string
exception NotArray of value
exception ArrayBounds of value * int
exception ElemNotFound of value
fun exnMessage e = General.exnMessage e
fun asBool (BOOL b) = b
  | asBool jv = raise NotBool jv
fun asInt (INT i) = IntInf.toInt i
  | asInt jv = raise NotInt jv
fun asIntInf (INT i) = i
  | asIntInf jv = raise NotInt jv
fun asNumber (FLOAT r) = r
  | asNumber (INT i) = Real.fromInt (IntInf.toInt i)
  | asNumber jv = raise NotNumber jv
fun asString (STRING s) = s
  | asString jv = raise NotString jv
fun findField (OBJECT flds) key =
    let
	fun f [] = NONE
	  | f ((key',jv')::flds) = if key = key' then SOME jv' else f flds
    in
	f flds
    end
  | findField jv _ = raise NotObject jv
fun lookupField jv key =
    case findField jv key
     of SOME jv' => jv'
      | NONE => raise FieldNotFound(jv, key)
fun hasField key (OBJECT flds) = Option.isSome (findField (OBJECT flds) key)
  | hasField _ _ = false
fun testField key pred (OBJECT flds) =
    (case findField (OBJECT flds) key
      of SOME jv => pred jv
       | NONE => false)
  | testField _ _ _ = false
fun asArray (ARRAY lst) = Vector.fromList lst
  | asArray jv = raise NotArray jv
fun arrayMap f (ARRAY lst) = map f lst
  | arrayMap _ jv = raise NotArray jv
    
datatype edge
  = SEL of string
  | SUB of int
  | FIND of value -> bool
type path = edge list

local
    fun get' (jv, []) = jv
      | get' (OBJECT jv, SEL s::p) = get' (lookupField (OBJECT jv) s, p)
      | get' (ARRAY lst, SUB i::p) =
	let
	    val jv = List.nth(lst, i)
		     handle Subscript => raise ArrayBounds(ARRAY lst, i)
	in
	    get' (jv, p)
	end
      | get' (ARRAY lst, FIND pred::p) =
	(case List.find pred lst
	 of SOME jv => get' (jv, p)
	 | NONE => raise ElemNotFound (ARRAY lst))
      | get' (jv, SEL _::_) = raise NotObject jv
      | get' (jv, SUB _::_) = raise NotArray jv
      | get' (jv, FIND _::_) = raise NotArray jv
in
fun get (jv, p) = get' (jv, p)
end
end

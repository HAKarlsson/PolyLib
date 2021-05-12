signature SET =
sig
    type key
    type t
    exception E of key

    val empty : t 
    val make : key list -> t
    val member : (t * key) -> bool
    val update : (t * key) -> t
end

signature SET =
sig
    type key
    type set

    val empty : set
    val make : key list -> set
    val member : (set * key) -> bool
    val update : (set * key) -> set
end

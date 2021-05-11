signature ORD_KEY =
sig
    type key
    val compare : key * key -> order
end
    
signature ORD_MAP =
sig
    structure K : ORD_KEY
    type 'a map

    val empty : 'a map
    val isEmpty : 'a map -> bool
    val size : 'a map -> int

    val keys : 'a map -> K.key list
    val values : 'a map -> 'a list
    val entries : 'a map -> (K.key * 'a) list

    val member : 'a map -> K.key -> bool
    val lookup : 'a map -> K.key -> 'a option
    val insert : (K.key * 'a) * 'a map -> 'a map
end

signature DICTIONARY =
sig
    type key
    type 'a dict 
    exception E of key

    val empty : 'a dict 
    val make : (key * 'a) list -> 'a dict
    val lookup : ('a dict * key) -> 'a option
    val update : ('a dict * key * 'a) -> 'a dict
end

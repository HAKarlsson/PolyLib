signature DICTIONARY =
sig
    type key
    type 'a t
    exception E of key

    val empty : 'a t 
    val make : (key * 'a) list -> 'a t
    val lookup : ('a t * key) -> 'a option
    val update : ('a t * key * 'a) -> 'a t
end

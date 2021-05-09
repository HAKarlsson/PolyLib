signature MAP =
sig
    type key
    type 'a entry
    type 'a map

    val empty : 'a map
    val size : 'a map -> int
    val keys : 'a map -> key list
    val values : 'a map -> 'a list
    val entries : 'a map -> ('a entry) list

    val member : 'a map -> key -> bool
    val lookup : 'a map -> key -> 'a option
    val insert : 'a entry * 'a map -> 'a map

    val prune : 'a map -> 'a map
end

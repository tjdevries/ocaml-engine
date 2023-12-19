type t [@@deriving compare, hash, sexp, eq]
type comparator_witness

val next : unit -> t
val comparator : (t, comparator_witness) Comparator.t

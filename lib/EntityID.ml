module T = struct
  type t = int [@@deriving compare, sexp, eq, hash]

  let entity_id = Atomic.make 0
  let next () = Atomic.fetch_and_add entity_id 1
end

include Comparator.Make (T)
include T

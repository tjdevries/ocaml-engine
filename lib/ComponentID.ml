(* TODO: Don't expose that this is an int *)
module T = struct
  type t = int [@@deriving compare, sexp, eq, hash]

  let component_id = ref 0

  let next () : t =
    incr component_id;
    !component_id
  ;;
end

include Comparator.Make (T)
include T

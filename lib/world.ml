module Entity : sig
  val next_id : unit -> int
end = struct
  let entity_id = Atomic.make 0
  let next_id () = Atomic.fetch_and_add entity_id 1
end

type t = { lookup : Component.Lookup.t }

let empty () =
  let lookup = Component.Lookup.empty () in
  { lookup }
;;

let combine_and_sequence left right =
  let rec do_next_thing left right =
    let next_left = Sequence.next left in
    let next_right = Sequence.next right in
    match next_left, next_right with
    | Some ((l_id, l_value), left), Some ((r_id, r_value), right) when l_id = r_id ->
      Some ((l_id, (l_value, r_value)), (left, right))
    | Some ((l_id, _), left), Some ((r_id, _), _) when l_id < r_id ->
      do_next_thing left right
    | Some ((l_id, _), _), Some ((r_id, _), right) when l_id > r_id ->
      do_next_thing left right
    | _ -> None
  in
  Sequence.unfold ~init:(left, right) ~f:(fun (left, right) -> do_next_thing left right)
;;

let combine_not_sequence query condition =
  let rec do_next_thing query condition =
    let next_query = Sequence.next query in
    let next_condition = Sequence.next condition in
    match next_query, next_condition with
    (* We don't have anything else that matches the condition, so just always return *)
    | Some (left_value, left), None -> Some (left_value, (left, condition))
    | Some ((l_id, _), query), Some ((r_id, _), condition) when l_id = r_id ->
      do_next_thing query condition
    | Some (((l_id, _) as left_value), left), Some ((r_id, _), _) when l_id < r_id ->
      Some (left_value, (left, condition))
    | Some ((l_id, _), _), Some ((r_id, _), condition) when l_id > r_id ->
      do_next_thing query condition
    | _ -> None
  in
  Sequence.unfold ~init:(query, condition) ~f:(fun (query, condition) ->
    do_next_thing query condition)
;;

let combine_with_sequence query condition =
  let rec do_next_thing left right =
    let next_left = Sequence.next left in
    let next_right = Sequence.next right in
    match next_left, next_right with
    | Some (((l_id, _) as left_value), left), Some ((r_id, _), right) when l_id = r_id ->
      Some (left_value, (left, right))
    | Some ((l_id, _), left), Some ((r_id, _), _) when l_id < r_id ->
      do_next_thing left right
    | Some ((l_id, _), _), Some ((r_id, _), right) when l_id > r_id ->
      do_next_thing left right
    | _ -> None
  in
  Sequence.unfold ~init:(query, condition) ~f:(fun (query, condition) ->
    do_next_thing query condition)
;;

let rec query_sequence : type a. t -> a Query.t -> (int * a) Sequence.t =
  fun t query ->
  let open Component in
  match query with
  | COMPONENT comp ->
    let module Comp = (val comp : COMPONENT with type t = a) in
    let store = Lookup.find t.lookup (module Comp) in
    Store.to_sequence store (module Comp)
  | AND (left, right) ->
    (* TODO: Would be better to iterate over this or make a sequence of the keys, to filter them there...*)
    let left = query_sequence t left in
    let right = query_sequence t right in
    combine_and_sequence left right
  | NOT { query; condition } ->
    let query_seq = query_sequence t query in
    let condition_seq = query_sequence t condition in
    combine_not_sequence query_seq condition_seq
  | WITH { query; condition } ->
    let query_seq = query_sequence t query in
    let condition_seq = query_sequence t condition in
    combine_with_sequence query_seq condition_seq
  | _ -> assert false
;;

(* TODO: Probably don't want to allow iterating over a query?... only want to iterate over a system? *)
let iter_query : type a. t -> a Query.t -> (a -> unit) -> unit =
  fun t query f -> query_sequence t query |> Sequence.iter ~f:(fun (_, value) -> f value)
;;

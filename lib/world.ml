[@@@ocaml.warning "-39-27-26"]

(*

   Systems generally specify when they should be run.
   - Some systems could say they are immutable (currently untype checked, don't think i have a way for this yet)
     - We have to wait for jane street to finish their modal stuff
     - If you have an "ImmutableTick" kind of event, you could actually run those all in parallel
*)

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

let rec to_hash : type a. t -> a Query.t -> (int, a) Hashtbl.t =
 fun t query ->
  let open Component in
  match query with
  | COMPONENT comp ->
      let module Comp = (val comp : COMPONENT with type t = a) in
      let store = Lookup.find t.lookup (module Comp) in
      Hashtbl.map store ~f:(fun data -> Comp.of_component data)
  | AND (left, right) ->
      (* TODO: Would be better to iterate over this or make a sequence of the keys, to filter them there...*)
      let left = to_hash t left in
      let possible_keys = Hashtbl.keys left in
      let right = to_hash_with_keys t right possible_keys in
      Hashtbl.mapi right ~f:(fun ~key ~data ->
          (Hashtbl.find_exn left key, data))
  | NOT { query; condition } ->
      let filter_keys = to_hash t condition |> Hashtbl.keys in
      (* DUMB WAY: should instead have a "condition" type function that gets passed *)
      let values = to_hash t query in
      Hashtbl.filter_keys_inplace values ~f:(fun key ->
          List.mem filter_keys key ~equal:Int.equal);
      values
  | WITH { query; condition } ->
      let filter_keys = to_hash t condition |> Hashtbl.keys in
      to_hash_with_keys t query filter_keys
  | _ -> assert false

and to_hash_with_keys : type a. t -> a Query.t -> int list -> (int, a) Hashtbl.t
    =
 fun t query keys ->
  let open Component in
  match query with
  | COMPONENT comp ->
      let module Comp = (val comp : COMPONENT with type t = a) in
      let store = Lookup.find t.lookup (module Comp) in
      let tbl = Hashtbl.create (module Int) in
      List.iter keys ~f:(fun key ->
          match Store.find store key with
          | Some component ->
              Hashtbl.set tbl ~key ~data:(Comp.of_component component)
          | None -> ());
      tbl
  | AND (left, right) ->
      (* Here's the problem though, you can see it here...
          we materialize an *entire* list of keys every time.
          That's bad! we should be using streams or sequences or something...*)
      let left = to_hash_with_keys t left keys in
      let keys = Hashtbl.keys left in
      let right = to_hash_with_keys t right keys in
      Hashtbl.mapi right ~f:(fun ~key ~data ->
          (Hashtbl.find_exn left key, data))
  | NOT { query; condition } -> assert false
  | _ -> assert false

let rec to_list : type a. t -> a Query.t -> (int * a) list =
 fun t query ->
  let open Query in
  let open Component in
  match query with
  | COMPONENT comp ->
      let module Comp = (val comp : Component.COMPONENT with type t = a) in
      Lookup.to_list t.lookup (module Comp)
  | _ ->
      let find_matching t query =
        match query with
        | COMPONENT comp ->
            let module Comp = (val comp : COMPONENT with type t = a) in
            Lookup.to_list t.lookup (module Comp)
        | _ -> assert false
      in
      let _matches = find_matching t query in
      let aux t query _ =
        match query with
        | COMPONENT comp ->
            let module Comp = (val comp : COMPONENT with type t = a) in
            Lookup.to_list t.lookup (module Comp)
        | _ -> assert false
      in
      aux t query []

let iter : type a. t -> a Query.t -> (int -> a -> unit) -> unit =
 fun t query f ->
  let hashed = to_hash t query in
  Hashtbl.iteri hashed ~f:(fun ~key ~data -> f key data)

(* let entity_store : int Bag.t = Bag.create () *)

(* module EntityStore = struct *)
(*   let remove_entity world id = *)
(*     World.iter world (fun id -> id); *)
(*     Bag.remove_one id |> ignore *)
(*   let add_entity world components = *)
(*     let id = Entity.next_id () in *)
(*     Bag.add_unit store id; *)
(*     World.associate world id components *)
(* end *)

let run : type a. t -> a System.t -> unit =
 fun t system -> iter t system.query system.f

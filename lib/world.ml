module Entity : sig
  val next_id : unit -> int
end = struct
  let entity_id = Atomic.make 0
  let next_id () = Atomic.fetch_and_add entity_id 1
end

module World = struct
  type t = { lookup : Component.Lookup.t }

  (* let entity_store : int Bag.t = Bag.create () *)
end

(* module EntityStore = struct *)
(*   let remove_entity world id = *)
(*     World.iter world (fun id -> id); *)
(*     Bag.remove_one id |> ignore *)
(*   let add_entity world components = *)
(*     let id = Entity.next_id () in *)
(*     Bag.add_unit store id; *)
(*     World.associate world id components *)
(* end *)

(* https://docs.rs/bevy/latest/bevy/ecs/prelude/struct.Commands.html *)
type command
type system

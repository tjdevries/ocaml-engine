let add_component (world : World.t) entity = function
  | Component.VALUE (component, value) ->
      Component.Lookup.set world.lookup component entity value

type t = { to_spawn : Bundle.t list; something : bool }

(* ... We don't want to do this right away. Need to
   wait til end of frame*)
let spawn t (bundle : Bundle.t) = { t with to_spawn = bundle :: t.to_spawn }

let execute t world =
  List.iter t.to_spawn ~f:(fun bundle ->
      let entity = World.Entity.next_id () in
      List.iter bundle.values ~f:(add_component world entity));

  (* TODO: Consider just using mutable or queue like structure here *)
  { t with to_spawn = [] }

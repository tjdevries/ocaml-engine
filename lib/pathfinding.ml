let make_query condition =
  Query.WITH { query = Query.COMPONENT (module Component.Position); condition }

let enemy_query = make_query (Query.COMPONENT (module Component.EnemyTag))
let player_query = make_query (Query.COMPONENT (module Component.PlayerTag))
(* let query = Query.BOTH (enemy_query, player_query) *)

(* (enemies, player) *)

(* let pathfinding_system = System.to_system { query = false } *)

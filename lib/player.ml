open Raylib
open Component

type player_stats = { mutable speed : float }

module PlayerStats = ComponentMaker (struct
  type t = player_stats
  type component += Stats of t

  let of_component = function Stats t -> t | _ -> failwith "bad value"
  let to_component t = Stats t
end)

let create add_to_player =
  let components =
    [
      mk_value (module PlayerTag) ();
      mk_value
        (module Sprite.T)
        { texture = Textures.player (); scale = 0.05; rotation = 1.0 };
      mk_value (module Position) (Vector2.create 15.0 15.0);
      mk_value (module Velocity) (Vector2.create 1.0 1.0);
      mk_value (module PlayerStats) { speed = 1.0 };
    ]
  in

  List.iter components ~f:add_to_player

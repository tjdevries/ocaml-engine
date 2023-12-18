open Raylib

type t = { values : Component.component_value list }

let make components = components

type player_bundle = {
  player_tag : unit;
  sprite : Sprite.T.t;
  position : Vector2.t;
  velocity : Vector2.t;
  stats : Player.player_stats;
}
[@@deriving bundle]

(*
   commands.spawn(PlayerBundle {
           name: PlayerName("Henry".into()),
           xp: PlayerXp(1000),
           health: Health {
               hp: 100.0, extra: 20.0
           },
           _p: Player,
           sprite: Default::default(),
       });
*)

let of_bundle ~sprite ~position ~velocity ~stats =
  { player_tag = (); sprite; position; velocity; stats }

let to_bundle player_bundle =
  let open Component in
  {
    values =
      [
        mk_value (module PlayerTag) player_bundle.player_tag;
        mk_value (module Sprite.T) player_bundle.sprite;
        mk_value (module Position) player_bundle.position;
        mk_value (module Velocity) player_bundle.velocity;
        mk_value (module Player.PlayerStats) player_bundle.stats;
      ];
  }

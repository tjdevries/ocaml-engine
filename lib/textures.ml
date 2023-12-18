open Raylib

let player_texture = ref None

let player () =
  match !player_texture with
  | Some t -> t
  | None ->
      let t = load_texture "resources/character.png" in
      player_texture := Some t;
      t

let mob_texture = ref None

let mob () =
  match !mob_texture with
  | Some t -> t
  | None ->
      let t = load_texture "resources/mob.png" in
      mob_texture := Some t;
      t

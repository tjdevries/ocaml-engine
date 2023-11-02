open Raylib

type t = { rect : Rectangle.t; texture : Texture2D.t; health : Health.t }

let health_bar_offset = Vector2.create 0.0 (-15.0)

let draw t =
  let center = Raytils.rectangle_to_vec2 t.rect in
  Health.draw t.health (Vector2.add center health_bar_offset);
  draw_texture_ex t.texture center 1.0 0.05 Color.white

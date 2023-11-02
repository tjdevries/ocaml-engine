open Raylib

type t = { mutable max_health : float; mutable cur_health : float }

let draw t point =
  (* Should these be part of the type?? *)
  let width = 50. in
  let height = 10. in
  let cur_width = t.cur_health /. t.max_health *. width in
  let remaining = width -. cur_width in
  let rect =
    Rectangle.create (Vector2.x point) (Vector2.y point) cur_width height
  in
  Raylib.draw_rectangle_rec rect Color.green;
  let rect =
    Rectangle.create
      (cur_width +. Vector2.x point)
      (Vector2.y point) remaining height
  in
  Raylib.draw_rectangle_rec rect Color.red

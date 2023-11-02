open Raylib

let rectangle_to_vec2 rectangle =
  let x = Rectangle.x rectangle +. (Rectangle.width rectangle /. 2.) in
  let y = Rectangle.y rectangle +. (Rectangle.height rectangle /. 2.) in
  Vector2.create x y

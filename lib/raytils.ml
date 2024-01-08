open Ctypes
open Foreign

let rectangle_to_vec2 rectangle =
  let open Raylib in
  let x = Rectangle.x rectangle +. (Rectangle.width rectangle /. 2.) in
  let y = Rectangle.y rectangle +. (Rectangle.height rectangle /. 2.) in
  Vector2.create x y
;;

module Vector2 = struct
  include Raylib.Vector2

  let to_tuple t = x t, y t
  let clamp = foreign "Vector2Clamp" (t @-> t @-> t @-> returning t)
  let clamp_value = foreign "Vector2ClampValue" (t @-> float @-> float @-> returning t)
end

module Rectangle = Raylib.Rectangle
module Color = Raylib.Color
module Camera2D = Raylib.Camera2D
module Texture2D = Raylib.Texture2D
module Key = Raylib.Key

let is_key_down = Raylib.is_key_down
let is_key_pressed = Raylib.is_key_pressed
let draw_texture_ex = Raylib.draw_texture_ex
let load_texture = Raylib.load_texture

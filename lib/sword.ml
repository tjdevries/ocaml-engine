open Raylib

let rectangle_to_vec2 = Raytils.rectangle_to_vec2

type status = Charging of int | Swinging of float

type t = {
  player : Player.t;
  length : float;
  width : float;
  speed : float;
  mutable status : status;
}

let update t =
  match t.status with
  | Charging time when time > 1 -> t.status <- Charging (time - 1)
  | Charging _ -> t.status <- Swinging 0.1
  | Swinging ratio when Float.(ratio >= 0.5) -> t.status <- Charging 100
  | Swinging ratio -> t.status <- Swinging (ratio +. 0.05)

let draw t =
  let position = rectangle_to_vec2 t.player.rect in
  match t.status with
  | Charging _ ->
      let point = Vector2.add position (Vector2.create 0. t.length) in
      draw_line_ex position point t.width Color.red
  | Swinging ratio ->
      draw_circle_sector position t.length 0.0 (ratio *. -360.) 100 Color.green

(* draw_line_ex position (Vector2.create 100. 100.) t.width Color.green *)

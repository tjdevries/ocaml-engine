open Raylib
open Component

type sprite = { texture : Texture2D.t; rotation : float; scale : float }

(* We only do 2D sprites, so this should be fine for now *)
module T = ComponentMaker (struct
  type t = sprite
  type component += Sprite of t

  let of_component = function Sprite t -> t | _ -> failwith "bad value"
  let to_component t = Sprite t
end)

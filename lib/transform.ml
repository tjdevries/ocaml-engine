open Component
open Raylib

(* TODO: GlobalTransform, which looks like transform but with private fields *)

(* pub struct Transform { *)
(*     pub translation: Vec3, *)
(*     pub rotation: Quat, *)
(*     pub scale: Vec3, *)
(* } *)
type t =
  { translation : Vector2.t
  ; rotation : float
  ; scale : Vector2.t
  }

module T = ComponentMaker (struct
    type nonrec t = t
    type component += Transform of t

    let of_component = function
      | Transform t -> t
      | _ -> failwith "bad value"
    ;;

    let to_component t = Transform t
  end)

let of_xy x y =
  { translation = Vector2.create x y; rotation = 0.0; scale = Vector2.zero () }
;;

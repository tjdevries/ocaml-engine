open Raytils
open Component

type player_stats = { mutable speed : float }

module PlayerStats = ComponentMaker (struct
    type t = player_stats
    type component += Stats of t

    let of_component = function
      | Stats t -> t
      | _ -> failwith "bad value"
    ;;

    let to_component t = Stats t
  end)

type player_bundle =
  { tag : unit [@PlayerTag]
  ; sprite : Sprite.t [@Sprite.T]
  ; transform : Transform.t [@Position]
  ; velocity : float * float [@Velocity]
  ; stats : player_stats [@PlayerStats]
  }
(* [@@deriving bundle] *)

(* TOOD: Generate this with ppx *)
let player_bundle ~sprite ~transform ~velocity ~stats =
  Bundle.make
    [ Component.mk_value (module PlayerTag) ()
    ; Component.mk_value (module Sprite.T) sprite
    ; Component.mk_value (module Transform.T) transform
    ; Component.mk_value (module Velocity) velocity
    ; Component.mk_value (module PlayerStats) stats
    ]
;;

let default () =
  player_bundle
    ~sprite:{ texture = Textures.player (); scale = 0.05; rotation = 1.0 }
    ~transform:(Transform.of_xy 15.0 15.0)
    ~velocity:(Vector2.create 1.0 1.0)
    ~stats:{ speed = 1.0 }
;;

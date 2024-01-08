open Component

let random_transform () =
  (* let x = if Random.bool () then -1.0 else 1.0 in *)
  (* let y = if Random.bool () then -1.0 else 1.0 in *)
  let x = 300.0 +. Random.float_range (-100.0) 100.0 in
  let y = 1.0 +. Random.float_range (-50.0) 50.0 in
  Transform.of_xy x y
;;

let random_mob_bundle () =
  Bundle.make
    [ mk_value
        (module Sprite.T)
        { texture = Textures.mob (); rotation = 1.0; scale = 0.03 }
    ; mk_value (module Transform.T) (random_transform ())
    ; mk_value (module Component.EnemyTag) ()
    ]
;;

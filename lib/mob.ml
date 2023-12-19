open Raylib
open Component

let random_location () =
  (* let x = if Random.bool () then -1.0 else 1.0 in *)
  (* let y = if Random.bool () then -1.0 else 1.0 in *)
  let x = 300.0 +. Random.float_range (-100.0) 100.0 in
  let y = 1.0 +. Random.float_range (-50.0) 50.0 in
  Vector2.create x y
;;

let random_mob_bundle () =
  Bundle.make
    [ mk_value
        (module Sprite.T)
        { texture = Textures.mob (); rotation = 1.0; scale = 0.03 }
    ; mk_value (module Component.Position) (random_location ())
    ; mk_value (module Component.EnemyTag) ()
    ]
;;

let create_random : (component_value -> unit) -> unit =
  fun add ->
  add
  @@ mk_value
       (module Sprite.T)
       { texture = Textures.mob (); rotation = 1.0; scale = 0.03 };
  add @@ mk_value (module Component.Position) (random_location ());
  add @@ mk_value (module Component.EnemyTag) ();
  ()
;;

(* { *)
(*   id = !mob_id; *)
(*   rect = *)
(*     Rectangle.create *)
(*       (x_modifier *. Random.float 800.0) *)
(*       (y_modifier *. 500.) 50.0 50.0; *)
(*   speed = 1.0 +. Random.float 1.5; *)
(*   health = { max_health = 10.; cur_health = 10. }; *)
(* } *)

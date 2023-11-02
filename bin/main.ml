[@@@ocaml.warning "-27-32-34-69"]

(* TODO: Find out if we can abuse algebraics effect & multicore to make this zoom zoom *)
(* TODO: Flyweight pattern (making sure to share textures, meshes, etc) *)
(*          Can use references to all the same object, which i think should work the same *)
(* TODO: instaced rendering -> https://en.wikipedia.org/wiki/Geometry_instancing *)
(* Stats tracking...
    Separate core that listens for events and does all the tracking based on that?
*)

open Core
open Raylib

(* Load in modules from our engine *)
module Health = Engine.Health
module Player = Engine.Player
module Sword = Engine.Sword

let rectangle_to_vec2 = Engine.Raytils.rectangle_to_vec2
let mob_id = ref 0

module Mob = struct
  type t = { id : int; rect : Rectangle.t; speed : float; health : Health.t }

  let random () =
    incr mob_id;
    let x_modifier = if Random.bool () then -1.0 else 1.0 in
    let y_modifier = if Random.bool () then -1.0 else 1.0 in
    {
      id = !mob_id;
      rect =
        Rectangle.create
          (x_modifier *. Random.float 800.0)
          (y_modifier *. 500.) 50.0 50.0;
      speed = 1.0 +. Random.float 1.5;
      health = { max_health = 10.; cur_health = 10. };
    }

  let take_damage t amount =
    t.health.cur_health <- t.health.cur_health -. amount;
    Float.(t.health.cur_health >= 0.)

  let draw t =
    let open Float in
    if t.health.cur_health > 5.0 then draw_rectangle_rec t.rect Color.green
    else draw_rectangle_rec t.rect Color.yellow
end

module Bullet = struct
  type t = {
    rect : Rectangle.t;
    damage : float;
    speed : float;
    direction : Vector2.t;
    mutable ticks_remaining : int;
    mutable pierce : int;
    mobs_hit : int Array.t;
  }

  let create ~rect ~damage ~speed ~direction ~ticks_remaining ~pierce =
    let mobs_hit = Array.create ~len:pierce 0 in
    { rect; damage; speed; direction; ticks_remaining; pierce; mobs_hit }

  let can_hit t (mob : Mob.t) =
    t.pierce > 0
    && check_collision_recs t.rect mob.rect
    && not (Array.mem t.mobs_hit mob.id ~equal:Int.equal)

  let hit_mob t (mob : Mob.t) =
    t.pierce <- t.pierce - 1;
    Array.set t.mobs_hit t.pierce mob.id;
    Mob.take_damage mob t.damage

  let draw t =
    (* draw_texture_ex t.texture (rectangle_to_vec2 t.rect) 1.0 0.05 Color.white *)
    draw_rectangle_rec t.rect Color.white

  let update t =
    if t.ticks_remaining <= 0 || t.pierce <= 0 then false
    else
      let _ = Vector2.lerp in

      Rectangle.set_x t.rect
        (Rectangle.x t.rect +. (Vector2.x t.direction *. t.speed));

      Rectangle.set_y t.rect
        (Rectangle.y t.rect +. (Vector2.y t.direction *. t.speed));

      t.ticks_remaining <- t.ticks_remaining - 1;

      true
end

module MobStore = struct
  type t = Mob.t Bag.t

  let empty () = Bag.create ()
  let add t mob = Bag.add_unit t mob
  let iter = Bag.iter
  let filter_inplace = Bag.filter_inplace
  let find = Bag.find
  let fold = Bag.fold
  let length = Bag.length
  let draw (t : t) = Bag.iter t ~f:Mob.draw
end

type state = {
  player : Player.t;
  camera : Camera2D.t;
  mobs : MobStore.t;
  bullets : Bullet.t list ref;
  sword : Sword.t;
  tick : int;
}

let width = 3840
let height = 2160
let screen_width = 1000

let setup () =
  let open Raylib in
  set_config_flags [ Window_resizable ];
  init_window width height "raylib [core] example - 2d camera";

  let texture = load_texture "resources/character.png" in
  let player =
    Player.
      {
        rect = Rectangle.create 0. 0. 40.0 40.0;
        texture;
        health = { max_health = 100.0; cur_health = 90.0 };
      }
  in
  let camera =
    Camera2D.create
      (Vector2.create (Float.of_int width /. 2.0) (Float.of_int height /. 2.0))
      (Vector2.create
         (Rectangle.x player.rect +. 20.0)
         (Rectangle.y player.rect +. 20.0))
      0.0 1.5
  in
  set_target_fps 60;
  (* Do lots of em *)
  let mobs = MobStore.empty () in
  for _ = 0 to 100 do
    MobStore.add mobs (Mob.random ())
  done;

  let sword =
    Sword.
      { player; length = 100.0; width = 5.; speed = 10.0; status = Charging 10 }
  in
  { player; camera; mobs; bullets = ref []; tick = 0; sword }

module RectUtils = struct
  let direction dir ~source ~target =
    if Float.(dir source > dir target) then -1.0
    else if Float.(dir source = dir target) then 0.0
    else 1.0

  let copy rect =
    Rectangle.create (Rectangle.x rect) (Rectangle.y rect)
      (Rectangle.width rect) (Rectangle.height rect)
end

let rec loop { player = player_t; camera; mobs; bullets; tick; sword } =
  let player = player_t.rect in
  match window_should_close () with
  | true -> close_window ()
  | false ->
      let open Raylib in
      let move_speed = 5.0 in
      (if is_key_down Key.Right then
         Rectangle.(set_x player (x player +. move_speed))
       else if is_key_down Key.Left then
         Rectangle.(set_x player (x player -. move_speed)));

      (if is_key_down Key.Down then
         Rectangle.(set_y player (y player +. move_speed))
       else if is_key_down Key.Up then
         Rectangle.(set_y player (y player -. move_speed)));

      Camera2D.set_target camera
        (Vector2.create
           (Rectangle.x player +. 20.0)
           (Rectangle.y player +. 20.0));

      (* Update mob positions *)
      let player_center = rectangle_to_vec2 player in
      MobStore.iter mobs ~f:(fun mob ->
          let mob_center = rectangle_to_vec2 mob.rect in
          let direction = Vector2.subtract player_center mob_center in
          let direction = Vector2.normalize direction in
          Rectangle.set_x mob.rect
            (Rectangle.x mob.rect +. (Vector2.x direction *. mob.speed));
          Rectangle.set_y mob.rect
            (Rectangle.y mob.rect +. (Vector2.y direction *. mob.speed)));

      Sword.update sword;

      MobStore.filter_inplace mobs ~f:(fun mob ->
          match
            List.find !bullets ~f:(fun bullet -> Bullet.can_hit bullet mob)
          with
          | Some bullet -> Bullet.hit_mob bullet mob
          | None -> true);

      begin_drawing ();

      (* Win condition!! *)
      if MobStore.length mobs = 0 then
        draw_text "YOU WIN" 100 100 30 Color.green
      else (
        (match
           MobStore.find mobs ~f:(fun mob ->
               check_collision_recs mob.rect player)
         with
        | Some _ -> close_window ()
        | None -> ());

        draw_fps 60 20;

        clear_background Color.black;
        begin_mode_2d camera;

        let size = screen_width in
        draw_rectangle (-size / 2) (-size / 2) size size Color.blue;

        (* let size = 100 in *)
        (* draw_rectangle (-size / 2) (-size / 2) size size Color.red; *)

        (* draw_rectangle_rec player Color.red; *)
        MobStore.draw mobs;
        List.iter !bullets ~f:Bullet.draw;

        Player.draw player_t;
        Sword.draw sword;

        (* Closest mob *)
        let () =
          match
            MobStore.fold mobs ~init:None ~f:(fun acc mob ->
                match acc with
                | Some acc ->
                    let center = rectangle_to_vec2 mob.rect in
                    let distance = Vector2.distance player_center center in
                    let acc_center = rectangle_to_vec2 Mob.(acc.rect) in
                    let acc_distance =
                      Vector2.distance player_center acc_center
                    in
                    if Float.(distance < acc_distance) then Some mob
                    else Some acc
                | None -> Some mob)
          with
          | Some closest ->
              let closest_center = rectangle_to_vec2 closest.rect in

              (* draw_line_ex player_center closest_center 10.0 Color.red; *)
              let direction =
                Vector2.normalize
                  (Vector2.subtract closest_center player_center)
              in

              if Float.(Stdlib.mod_float (of_int tick) 30.0 = 0.0) then (
                let rect = RectUtils.copy player in
                Rectangle.set_height rect 15.0;
                Rectangle.set_width rect 15.0;
                bullets :=
                  Bullet.create ~rect ~damage:6.0 ~speed:8.0 ~direction
                    ~ticks_remaining:600 ~pierce:15
                  :: !bullets)
          | None -> ()
        in

        bullets := List.filter !bullets ~f:Bullet.update
        (* draw_rectangle_rec closest.rect Color.blue; *)

        (* draw_texture_ex player_t.texture (Vector2.create 400. 280.) 0.0 0.1 *)
        (*   Color.white; *));
      end_mode_2d ();

      draw_text "Free 2d camera controls:" 20 20 10 Color.black;
      draw_text "- Right/Left to move Offset" 40 40 10 Color.darkgray;
      draw_text "- Mouse Wheel to Zoom in-out" 40 60 10 Color.darkgray;
      draw_text "- A / S to Rotate" 40 80 10 Color.darkgray;
      draw_text "- R to reset Zoom and Rotation" 40 100 10 Color.darkgray;
      end_drawing ();

      loop { player = player_t; camera; mobs; bullets; tick = tick + 1; sword }

let () = setup () |> loop

open Raylib

let width = 3840
let height = 2160
let screen_width = 1000

type state =
  { world : World.t
  ; camera : Camera2D.t
  }

let setup () =
  set_config_flags [ Window_resizable ];
  init_window width height "raylib [core] example - 2d camera";
  set_target_fps 60;
  let world = World.empty () in
  let add_component entity = function
    | Component.VALUE (component, value) ->
      Component.Lookup.set world.lookup component entity value
  in
  Player.create (add_component @@ World.Entity.next_id ());
  Mob.create_random (add_component @@ World.Entity.next_id ());
  (* let _ = World.Entity.next_id () in *)
  let camera =
    Camera2D.create
      (Vector2.create (Float.of_int width /. 2.0) (Float.of_int height /. 2.0))
      (Vector2.create 0.0 0.0)
      0.0
      1.5
  in
  { world; camera }
;;

let player_query = Query.COMPONENT (module Component.PlayerTag)
let enemy_query = Query.component (module Component.EnemyTag)
let player_stats_query = Query.COMPONENT (module Player.PlayerStats)
let position_query = Query.COMPONENT (module Component.Position)
let velocity_query = Query.COMPONENT (module Component.Velocity)
let sprite_query = Query.COMPONENT (module Sprite.T)
let drawable_query = Query.AND (position_query, sprite_query)

let stats_system =
  System.iter player_stats_query (fun stats ->
    if is_key_down Key.Space then stats.speed <- stats.speed +. 1.0)
;;

(* something *)
let keyboard_system =
  System.iter
    (Query.WITH
       { query = Query.AND (velocity_query, player_stats_query)
       ; condition = player_query
       })
    (fun (velocity, stats) ->
      let speed = stats.speed in
      (if is_key_down Key.Right
       then Vector2.(set_x velocity speed)
       else if is_key_down Key.Left
       then Vector2.(set_x velocity (-1.0 *. speed))
       else Vector2.(set_x velocity 0.0));
      if is_key_down Key.Down
      then Vector2.(set_y velocity speed)
      else if is_key_down Key.Up
      then Vector2.(set_y velocity (-1.0 *. speed))
      else Vector2.(set_y velocity 0.0))
;;

let position_system =
  System.iter
    (Query.AND (velocity_query, position_query))
    (fun (velocity, position) ->
      (* TODO: Could filter out velocity that is 0? *)
      let new_position = Vector2.(add position velocity) in
      Vector2.set_x position (Vector2.x new_position);
      Vector2.set_y position (Vector2.y new_position);
      ())
;;

let drawable_system =
  System.iter drawable_query (fun (position, sprite) ->
    draw_texture_ex sprite.texture position sprite.rotation sprite.scale Color.white)
;;

let collision_system =
  let open System in
  let enemies = Query.WITH { query = position_query; condition = enemy_query } in
  let players = Query.WITH { query = position_query; condition = player_query } in
  let queries = QueryList.combine (QueryList.query enemies) (QueryList.query players) in
  let system = System.{ queries } in
  System.make system (fun world _ ->
    let _, enemy_pos =
      World.query_sequence world enemies |> Sequence.to_list |> List.hd_exn
    in
    let _, player_pos =
      World.query_sequence world players |> Sequence.to_list |> List.hd_exn
    in
    Fmt.pr "Distance: %f@." (Vector2.distance enemy_pos player_pos);
    if Float.(Vector2.distance enemy_pos player_pos < 5.0)
    then Fmt.failwith "OH YA COLLUISIONS BABY";
    ())
;;

let systems =
  collision_system
  :: stats_system
  :: keyboard_system
  :: position_system
  :: drawable_system
  :: System.systems
;;

let rec loop state =
  match window_should_close () with
  | true -> close_window ()
  | false ->
    begin_drawing ();
    begin_mode_2d state.camera;
    let size = screen_width in
    draw_rectangle (-size / 2) (-size / 2) size size Color.blue;
    List.iter systems ~f:(fun (module Sys) -> Sys.execute state.world Sys.query);
    (* Sys.execute results); *)
    (* World.execute_commands state.world |> ignore; *)
    end_mode_2d ();
    end_drawing ();
    loop state
;;

let main () = setup () |> loop

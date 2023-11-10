open Raylib

let width = 3840
let height = 2160
let screen_width = 1000

type state = { world : World.t; player : int }

let setup () =
  set_config_flags [ Window_resizable ];
  init_window width height "raylib [core] example - 2d camera";
  set_target_fps 60;

  let world = World.empty () in

  (* TODO: This should be a bundle so you can't forget any of the pieces! *)
  let player = World.Entity.next_id () in

  (* Mark the player... as a player! *)
  Component.Lookup.set world.lookup (module Component.PlayerTag) player ();

  (* So cool! Set a texture as a component!! *)
  let texture = load_texture "resources/character.png" in
  Component.Lookup.set world.lookup (module Component.Sprite) player texture;

  let position = Vector2.create 15.0 15.0 in
  Component.Lookup.set world.lookup (module Component.Position) player position;

  (* let camera = *)
  (*   Camera2D.create *)
  (*     (Vector2.create (Float.of_int width /. 2.0) (Float.of_int height /. 2.0)) *)
  (*     (Vector2.create *)
  (*        (Rectangle.x player.rect +. 20.0) *)
  (*        (Rectangle.y player.rect +. 20.0)) *)
  (*     0.0 1.5 *)
  (* in *)
  { world; player }

let position_query = Query.COMPONENT (module Component.Position)
let sprite_query = Query.COMPONENT (module Component.Sprite)
let drawable_query = Query.AND (position_query, sprite_query)

let rec loop state =
  match window_should_close () with
  | true -> close_window ()
  | false ->
      begin_drawing ();

      let size = screen_width in
      draw_rectangle (-size / 2) (-size / 2) size size Color.blue;

      let result =
        Query.query_lookup state.world.lookup drawable_query state.player
      in
      let _ =
        result
        |> Option.map ~f:(fun (position, sprite) ->
               draw_texture_ex sprite position 1.0 0.05 Color.white)
      in

      end_drawing ();
      loop state

let main () = setup () |> loop

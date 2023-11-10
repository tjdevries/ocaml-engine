open Raylib

type t = { rect : Rectangle.t; texture : Texture2D.t; health : Health.t }

let health_bar_offset = Vector2.create 0.0 (-15.0)

let draw t =
  let center = Raytils.rectangle_to_vec2 t.rect in
  Health.draw t.health (Vector2.add center health_bar_offset);
  draw_texture_ex t.texture center 1.0 0.05 Color.white

type component = ..

(*  Components *)
type component +=
  | Health of int
  | Position of Vector2.t
  | Texture of Texture2D.t
  | Velocity of Vector2.t
  | IsPlayer

(* Bundle *)
let x = [ Health 5; Velocity (Vector2.create 1.0 0.0) ]

module Entity = struct end

type component += Vision of float

let y = [ Health 5; Velocity (Vector2.create 1.0 0.0); Vision 10.0 ]

(* we can make a ppx (aka macro) to do special stuff *)
(* or we could possibly use functors (functions on top of modules in ocaml, different than haskLUL) *)

type world
type entity

module type Query = sig
  val components : component list
  val query : world -> entity list
end

(* let (module Something) = Query with components = x end *)
(* let%query check_zero_health (Health, Velocity, ...) =  *)

(* let zero_health_query = module Query with  *)

(* Obj.Extension_constructor.of_val *)

module type Component = sig
  type t
end

let vec = Vector2.create 1.0 1.0

(* let x = (Health 10, (Position vec, (IsPlayer, ()))) *)
(* let x = Health 10 :: Position vec :: IsPlayer *)

(* let%query my_query = Health, Position, IsPlayer *)

type position_query = { health : Health.t; is_player : bool } [@@deriving query]

module Position = struct
  type t = { x : int; y : int } [@@deriving query]

  (* include (Query with type t = t) *)
end

(* created by the ppx... *)
let position_query world f =
  world
  |> Seq.filter (fun _ -> true (* do the matches from the query names *))
  |> Seq.iter f

let my_func query =
  Seq.iter
    (fun (_health, position, _player) ->
      print_endline position;
      assert false)
    query

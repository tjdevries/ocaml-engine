(* TODO: how to use systems to do this... *)

(* TODO: How do we make Archetypes...
    Archetypes are combinations of components that (sometimes common ones, sometimes always) hold
    data together which let's you go really zoom zoom when doing  stuff.

    You just run through an array and do stuff :)

   Might not need archetypes if everything else is fast enough
*)

type 'a t = { query : 'a Query.t; f : int -> 'a -> unit }

(* TODO: make a SystemMaker or something like that so I don't have to write the type t *)
module type SYSTEM = sig
  type t

  val query : t Query.t
  val f : int -> t -> unit
end

module PlayerSystem : SYSTEM = struct
  type t = unit

  let query = Query.component (module Component.PlayerTag)
  let f _ _ = print_endline "found a player!"
end

module HealthSystem : SYSTEM = struct
  type t = float

  let query = Query.component (module Component.Health)
  let f id health = Fmt.pr "found some health: %d -> %f" id health
end

let systems : (module SYSTEM) list =
  [ (module PlayerSystem); (module HealthSystem) ]

let player_tag =
  {
    query = Query.component (module Component.PlayerTag);
    f = (fun _ _ -> print_endline "found a player!");
  }

let health =
  {
    query = Query.component (module Component.Health);
    f = (fun id health -> Fmt.pr "found some health: %d -> %f" id health);
  }

module SystemList = struct
  type ('ty, 'v) t =
    | [] : ('v, 'v) t
    | ( :: ) : 'a * ('ty, 'v) t -> ('a -> 'ty, 'v) t

  let cons x l = x :: l
  let one x = [ x ]

  let rec append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
   fun l1 l2 -> match l1 with [] -> l2 | h :: t -> h :: append t l2

  (* let rec iter : type a b. (a, b) t -> (a -> unit) -> unit = *)
  (*  fun l f -> *)
  (*   match l with *)
  (*   | [] -> () *)
  (*   | h :: t -> *)
  (*       f h; *)
  (*       iter f t *)
end

let x : _ SystemList.t = SystemList.[ player_tag; health ]

(* type system = .. *)

(* let run (s : 'a system) (a : 'a) : unit = s.f a *)
(* World.iter state.world *)
(*   (Query.WITH { query = drawable_query; condition = player_query }) *)
(*   (fun _ (position, sprite) -> *)
(*     draw_texture_ex sprite position 1.0 0.05 Color.white); *)

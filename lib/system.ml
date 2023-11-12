(* TODO: How do we make Archetypes...
    Archetypes are combinations of components that (sometimes common ones, sometimes always) hold
    data together which let's you go really zoom zoom when doing  stuff.

    You just run through an array and do stuff :)

   Might not need archetypes if everything else is fast enough
*)

type 'a t = { query : 'a Query.t; f : int -> 'a -> unit }

module type SYSTEM = sig
  type t

  val query : t Query.t
  val f : int -> t -> unit
end

(* Alternative to not have `type 'a t` for the system... *)
(* let to_system : type a. a Query.t -> (int -> a -> unit) -> (module SYSTEM) = *)
(*  fun query f -> *)
(*   (module struct *)
(*     type t = a *)
(*     let query = query *)
(*     let f = f *)
(*   end) *)

let to_system : type a. a t -> (module SYSTEM) =
 fun system ->
  (module struct
    type t = a

    let query = system.query
    let f = system.f
  end)

let player_system =
  to_system
    {
      query = Query.component (module Component.PlayerTag);
      f = (fun _ _ -> print_endline "found a player!");
    }

let health_system =
  to_system
    {
      query = Query.component (module Component.Health);
      f = (fun id health -> Fmt.pr "found some health: %d -> %f" id health);
    }

let systems : (module SYSTEM) list = [ player_system; health_system ]

(* TODO: How do we make Archetypes...
   Archetypes are combinations of components that (sometimes common ones, sometimes always) hold
   data together which let's you go really zoom zoom when doing  stuff.

   You just run through an array and do stuff :)

   Might not need archetypes if everything else is fast enough
*)

module QueryList = struct
  type 'a t =
    | QUERY : 'a Query.t -> 'a t
    | COMBINE : ('a t * 'b t) -> ('a * 'b) t

  let query a = QUERY a
  let combine a b = COMBINE (a, b)
end

type 'q t =
  { queries : 'q
  (* ; resources : 'a resource list *)
  (* ; event_writers : 'a *)
  (* ; event_readers : 'a *)
  }

module type SYSTEM = sig
  type query

  val query : query t
  val execute : World.t -> query t -> unit
end

let single_system
  : type a. a Query.t -> (a -> unit) -> (module SYSTEM with type query = a)
  =
  fun query _system ->
  (module struct
    type query = a

    let query = { queries = QueryList.QUERY query }

    (* TODO: I don't know what I want to write for this part yet. *)
    let execute _ = assert false
  end)
;;

let make : type a. a t -> (World.t -> a t -> unit) -> (module SYSTEM) =
  fun t f ->
  (module struct
    type query = a

    let query = t
    let execute = f
  end)
;;

let iter : type a. a Query.t -> (a -> unit) -> (module SYSTEM) =
  fun query f ->
  (module struct
    type query = a

    let query = { queries = QueryList.QUERY query }

    (* TODO: I don't know what I want to write for this part yet. *)
    let execute world { queries; _ } =
      match queries with
      | QueryList.QUERY query -> World.iter_query world query f
      | _ -> assert false
    ;;
  end)
;;

let systems : (module SYSTEM) list = []

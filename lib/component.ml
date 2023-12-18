open Core

(* TODO: Don't expose that this is an int *)
module ComponentID = struct
  type t = int

  let component_id = ref 0

  let next () : t =
    incr component_id;
    !component_id
  ;;
end

type component = ..
type component += Tag of unit

module type COMPONENT = sig
  type t

  val id : ComponentID.t
  val of_component : component -> t
  val to_component : t -> component
end

module type COMPONENT_BASE = sig
  type t

  val of_component : component -> t
  val to_component : t -> component
end

module ComponentMaker (Base : COMPONENT_BASE) : COMPONENT with type t = Base.t = struct
  (*  Load base *)
  include Base

  (* Create a new component id, to ensure that every component has unique id *)
  let id : ComponentID.t = ComponentID.next ()
end

module TagMaker () : COMPONENT with type t = unit = struct
  include ComponentMaker (struct
      type t = unit

      let of_component = function
        | Tag () -> ()
        | _ -> failwith "TagMaker failure"
      ;;

      let to_component () = Tag ()
    end)
end


(** A store contains a mapping of entity ids -> component values.
    These components are all the *same* COMPONENT, which is enforced
    by good coding (hopefully). *)
module Store = struct
  type t = (int, component, Int.comparator_witness) Map.t

  let empty () : t = Map.empty (module Int)
  let find = Map.find
  let map = Map.map
  let remove = Map.remove
  let iteri = Map.iteri
  let set t key data = Map.set t ~key ~data

  let to_sequence
    : type a. t -> (module COMPONENT with type t = a) -> (ComponentID.t * a) Sequence.t
    =
    fun t (module Comp) ->
    let seq = Map.to_sequence t in
    Sequence.map ~f:(fun (id, value) -> id, Comp.of_component value) seq
  ;;
end

(** Lookup contains a map of component_id to Store,
    which is used to go from a COMPONENT to a Store, which can
    then be used to lookup entity values. *)
module Lookup = struct
  type t = (ComponentID.t, Store.t) Hashtbl.t

  let empty () : t = Hashtbl.create (module Int)

  let add_component t (module Comp : COMPONENT) =
    let store = Store.empty () in
    Hashtbl.set t ~key:Comp.id ~data:store;
    store
  ;;

  (* TODO: Hide this function, should not be exposed *)
  let find t (module Comp : COMPONENT) =
    match Hashtbl.find t Comp.id with
    | Some store -> store
    | None -> add_component t (module Comp)
  ;;

  let retrieve : type a. t -> (module COMPONENT with type t = a) -> int -> a option =
    fun t (module Comp) id ->
    let store = find t (module Comp) in
    Store.find store id |> Option.map ~f:Comp.of_component
  ;;

  let set : type a. t -> (module COMPONENT with type t = a) -> int -> a -> unit =
    fun t (module Comp) id value ->
    let store = find t (module Comp) in
    let store = Store.set store id (Comp.to_component value) in
    Hashtbl.set t ~key:Comp.id ~data:store
  ;;

  (** Remove an entity from the lookup, deletes all associated components *)
  let remove_entity t id =
    Hashtbl.iter t ~f:(fun store ->
      (* TODO: Modifying while iterating... just modifying values, so hopefully fine? *)
      let store = Store.remove store id in
      Hashtbl.set t ~key:id ~data:store)
  ;;

  let to_list : type a. t -> (module COMPONENT with type t = a) -> (int * a) list =
    fun t (module Comp) ->
    let store = find t (module Comp) in
    Map.to_alist store
    |> List.map ~f:(fun (id, component) -> id, Comp.of_component component)
  ;;

  (** Iterates through all components and finds matching entity sets *)
  (* let iter (t : t) (query : 'a Query.t) : 'a list = *)
  (*   (* Hashtbl.iter *) *)
  (*   [] *)
end

type component_value =
  | VALUE : (module COMPONENT with type t = 'a) * 'a -> component_value

let mk_value : type a. (module COMPONENT with type t = a) -> a -> component_value =
  fun component value -> VALUE (component, value)
;;

(** Marks the entity as a player *)
module PlayerTag = TagMaker ()

(** Marks the entity as an enemy *)
module EnemyTag = TagMaker ()

module CameraTag = TagMaker ()
(* Marks the entity as a camera *)

module Health = ComponentMaker (struct
    type t = float
    type component += Health of t

    let of_component = function
      | Health t -> t
      | _ -> failwith "bad value"
    ;;

    let to_component t = Health t
  end)

module Position = ComponentMaker (struct
    type t = Raylib.Vector2.t
    type component += Vec of t

    let of_component = function
      | Vec t -> t
      | _ -> failwith "bad value"
    ;;

    let to_component t = Vec t
  end)

module Velocity = ComponentMaker (struct
    type t = Raylib.Vector2.t
    type component += Vec of t

    let of_component = function
      | Vec t -> t
      | _ -> failwith "bad value"
    ;;

    let to_component t = Vec t
  end)

open Core

type component = ..

(* Default "Tag" type component *)
type component += Tag of unit

(* TODO: Don't expose that this is an int... i think via .mli it's fine *)
type component_id = int

module type COMPONENT = sig
  type t

  val id : component_id
  val of_component : component -> t
  val to_component : t -> component
end

module type COMPONENT_BASE = sig
  type t

  val of_component : component -> t
  val to_component : t -> component
end

let component_id = ref 0

module ComponentMaker (Base : COMPONENT_BASE) : COMPONENT with type t = Base.t =
struct
  (*  Load base *)
  include Base

  (* Create a new component id, to ensure that every component has unique id *)
  let id : component_id =
    incr component_id;
    !component_id
end

module TagMaker () : COMPONENT = struct
  include ComponentMaker (struct
    type t = unit

    let of_component = function Tag () -> () | _ -> failwith "baddies"
    let to_component () = Tag ()
  end)
end

module Store = struct
  type t = (int, component) Hashtbl.t

  let empty () : t = Hashtbl.create (module Int)
  let find = Hashtbl.find
  let set t key data = Hashtbl.set t ~key ~data
end

module Lookup = struct
  type t = (component_id, Store.t) Hashtbl.t

  let empty () : t = Hashtbl.create (module Int)

  let add_component t (module Comp : COMPONENT) =
    let store = Store.empty () in
    Hashtbl.set t ~key:Comp.id ~data:store;
    store

  (* TODO: Hide this funciton, should not be exposed *)
  let find t (module Comp : COMPONENT) =
    match Hashtbl.find t Comp.id with
    | Some store -> store
    | None -> add_component t (module Comp)

  let retrieve :
      type a. t -> (module COMPONENT with type t = a) -> int -> a option =
   fun t (module Comp) id ->
    let store = find t (module Comp) in
    Store.find store id |> Option.map ~f:Comp.of_component

  let set : type a. t -> (module COMPONENT with type t = a) -> int -> a -> unit
      =
   fun t (module Comp) id value ->
    let store = find t (module Comp) in
    Store.set store id (Comp.to_component value)
end

module PlayerTag = TagMaker ()

module Health = ComponentMaker (struct
  type t = float
  type component += Health of t

  let of_component = function Health t -> t | _ -> failwith "bad value"
  let to_component t = Health t
end)

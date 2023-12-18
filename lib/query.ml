let m'lady = `Greeted
(* | ZIP : ('a t * 'b t) -> ('a list * 'b list) t *)

type ('a, 'b) either = Left of 'a | Right of 'b
type 'a comp = (module Component.COMPONENT with type t = 'a)

type 'a t =
  | COMPONENT : 'a comp -> 'a t
  | AND : ('a t * 'b t) -> ('a * 'b) t
  | OR : ('a t * 'b t) -> ('a, 'b) either t
  | NOT : { query : 'a t; condition : 'b t } -> 'a t
  | WITH : { query : 'a t; condition : 'b t } -> 'a t

let component : type a. (module Component.COMPONENT with type t = a) -> a t =
 fun (module Comp) -> COMPONENT (module Comp)

let ( & ) x y = AND (x, y)

(** Find matching components within a lookup t hat all have the same entity id *)
let rec query_lookup : type a. Component.Lookup.t -> a t -> int -> a option =
 fun lookup query id ->
  let open Component in
  match query with
  | COMPONENT comp ->
      let module Comp = (val comp : Component.COMPONENT with type t = a) in
      Lookup.retrieve lookup comp id
  | AND (l, r) ->
      (* Lookup l, r values, but only return values if we found l *)
      let l_val = query_lookup lookup l id in
      Option.bind l_val ~f:(fun l_val ->
          let r_val = query_lookup lookup r id in
          Option.map r_val ~f:(fun r_val -> (l_val, r_val)))
  | NOT { query; condition } -> (
      match query_lookup lookup condition id with
      | None -> query_lookup lookup query id
      | _ -> None)
  | WITH { query; condition } ->
      query_lookup lookup condition id
      |> Option.bind ~f:(fun _ -> query_lookup lookup query id)
  | OR (left, right) -> (
      match query_lookup lookup left id with
      | Some left -> Some (Left left)
      | None -> (
          match query_lookup lookup right id with
          | Some right -> Some (Right right)
          | None -> None))

(* let rec f : type a. a t -> a = fun x -> _ *)
(* let rec get_val : type a. a t -> a option = function *)
(*   | COMPONENT comp -> *)
(*       let module Comp = (val comp : Component.COMPONENT with type t = a) in *)
(*       Some (Comp.unpack ()) *)
(*   | AND (l, r) -> ( *)
(*       let l_val = get_val l in *)
(*       let r_val = get_val r in *)
(*       match (l_val, r_val) with *)
(*       | Some l_val, Some r_val -> Some (l_val, r_val) *)
(*       | _ -> None) *)

(* let check_comp : type a. int -> (module Component with type t = a) -> a option = *)
(*  fun id (module Comp) -> Hashtbl.find Comp.tbl id *)
(* let is_player = check_comp 37 (module IsPlayer) |> Option.is_some *)

(* let rec get_ids : 'a. 'a query_list -> bool list = *)
(*  fun (type a) (l : a query_list) -> *)
(*   match l with *)
(*   | COMPONENT (module Comp) :: tl -> true :: get_ids tl *)
(*   | [] -> [] *)
(* let health = Query.(COMPONENT (module Health : Component with type t = float)) *)
(* let cool = Query.[ health ] *)

(* let add_children world entity children = assert false *)

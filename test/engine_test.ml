open Core
open Engine

module Example = Component.ComponentMaker (struct
    type t = bool
    type Component.component += Example of t

    let of_component = function
      | Example b -> b
      | _ -> failwith "NO"
    ;;

    let to_component b = Example b
  end)

(* TODO: Seems... not good that i have to put this here? but i'm not sure how else to expose... *)
type record =
  { cool : bool
  ; number : float
  }

module WithRecord = Component.ComponentMaker (struct
    type t = record
    type Component.component += WithRecord of t

    let of_component = function
      | WithRecord b -> b
      | _ -> failwith "NO"
    ;;

    let to_component b = WithRecord b
  end)

(* Some example IDs *)
let first_id = EntityID.next ()
let second_id = EntityID.next ()
let third_id = EntityID.next ()

let test_lookup () =
  let open Component in
  let lookup = Lookup.empty () in
  Lookup.set lookup (module Example) first_id true;
  Lookup.set lookup (module Example) second_id false;
  Lookup.set lookup (module Health) first_id 1.0;
  Lookup.set lookup (module WithRecord) first_id { cool = true; number = 1.0 };
  let value = Lookup.retrieve lookup (module Example) first_id in
  (match value with
   | Some true -> ()
   | _ -> failwith "NO");
  let value = Lookup.retrieve lookup (module Health) first_id in
  (match value with
   | Some 1.0 -> ()
   | _ -> failwith "NO");
  let value = Lookup.retrieve lookup (module WithRecord) first_id in
  (match value with
   | Some { cool = true; number = 1.0 } -> ()
   | _ -> failwith "DID NOT MATCH");
  let value = Lookup.retrieve lookup (module Example) second_id in
  (match value with
   | Some false -> ()
   | _ -> failwith "NO");
  let value = Lookup.retrieve lookup (module Example) third_id in
  (match value with
   | None -> ()
   | _ -> failwith "NO");
  ()
;;

let test_query () =
  let open Component in
  let lookup = Lookup.empty () in
  (* Fill with some data *)
  Lookup.set lookup (module Example) first_id true;
  Lookup.set lookup (module Health) first_id 1.0;
  Lookup.set lookup (module Example) second_id false;
  Lookup.set lookup (module Health) second_id 13.0;
  Lookup.set lookup (module Example) third_id false;
  (* Create the queries *)
  let x = Query.component (module Example) in
  let y = Query.component (module Health) in
  let both = Query.AND (x, y) in
  (*  Do some querying *)
  let result = Query.query_lookup lookup both first_id in
  (match result with
   | Some (true, 1.0) -> ()
   | _ -> failwith "Could not with match this");
  let result = Query.query_lookup lookup both second_id in
  (match result with
   | Some (false, 13.0) -> ()
   | _ -> failwith "Could not with match this");
  (* Confirm that adding the `health *)
  let () =
    (match Query.query_lookup lookup both third_id with
     | None -> ()
     | _ -> failwith "Should not find this");
    Lookup.set lookup (module Health) third_id 3.0;
    match Query.query_lookup lookup both third_id with
    | Some (false, 3.0) -> ()
    | _ -> failwith "Should not find this"
  in
  ()
;;

let test_not_query () =
  let open Component in
  let lookup = Lookup.empty () in
  (* 1 has Example and Health *)
  Lookup.set lookup (module Example) first_id true;
  Lookup.set lookup (module Health) first_id 1.0;
  (* second_id has just Example *)
  Lookup.set lookup (module Example) second_id true;
  let open Query in
  let example_query = component (module Example) in
  let not_query = NOT { query = example_query; condition = component (module Health) } in
  let should_be_none = query_lookup lookup not_query first_id in
  (match should_be_none with
   | None -> ()
   | _ -> failwith "Should have been none");
  let should_be_some = query_lookup lookup not_query second_id in
  (match should_be_some with
   | Some true -> ()
   | _ -> failwith "Should have been none");
  ()
;;

let test_iter_with_query () =
  let open Component in
  let world = World.empty () in
  let lookup = world.lookup in
  (* 1 has Example and Health *)
  Lookup.set lookup (module Example) first_id true;
  Lookup.set lookup (module Health) first_id 22.0;
  (* second_id has just Example *)
  Lookup.set lookup (module Example) second_id true;
  (* third_id has just Health *)
  Lookup.set lookup (module Health) third_id 20.0;
  let open Query in
  let example_query = component (module Example) in
  let health_query = component (module Health) in
  let counter = ref 0 in
  World.iter_query world example_query (fun _ -> incr counter);
  if !counter <> 2 then failwith "Failed to count the examples";
  let counter = ref 0.0 in
  World.iter_query world health_query (fun health -> counter := !counter +. health);
  if Float.(!counter <> 42.0) then failwith "Failed to count the health components";
  let length = World.query_sequence world example_query |> Sequence.length in
  if length <> 2 then failwith "Failed to get all the examples";
  ()
;;

let test_iter_with_and_query () =
  let open Component in
  let world = World.empty () in
  let lookup = world.lookup in
  (* 1 has Example and Health *)
  Lookup.set lookup (module Example) first_id true;
  Lookup.set lookup (module Health) first_id 22.0;
  (* 2 has just Example *)
  Lookup.set lookup (module Example) second_id true;
  (* third_id has just Health *)
  Lookup.set lookup (module Health) third_id 20.0;
  let open Query in
  let example_query = component (module Example) in
  let health_query = component (module Health) in
  let both = Query.AND (example_query, health_query) in
  let seq = World.query_sequence world both in
  let result = Sequence.nth seq 0 in
  if Option.is_none result then failwith "Failed to get BOTH values";
  let _, result = Option.value_exn result in
  (match result with
   | true, 22.0 -> ()
   | _ -> failwith "Not the right values for BOTH");
  (* Add Health to entity 2 *)
  Lookup.set lookup (module Health) second_id 22.0;
  let count = World.query_sequence world both |> Sequence.length in
  if count <> 2 then failwith "Failed to find NEWLY ADDED both value";
  ()
;;

let test_iter_with_and_query_perf () =
  let open Component in
  let world = World.empty () in
  let lookup = world.lookup in
  (* 1 has Example and Health *)
  let player = first_id in
  Lookup.set lookup (module Example) player true;
  Lookup.set lookup (module PlayerTag) player ();
  Lookup.set lookup (module Health) player 22.0;
  (* 2 has Example and Health *)
  let enemy = second_id in
  Lookup.set lookup (module Example) enemy true;
  Lookup.set lookup (module Health) enemy 22.0;
  (* Make a lot of other entities *)
  for i = 3 to 1_000_000 do
    let id = EntityID.next () in
    Lookup.set lookup (module Health) id (Float.of_int i)
  done;
  let open Query in
  let example_query = component (module Example) in
  let player_query = component (module PlayerTag) in
  let health_query = component (module Health) in
  let query = Query.AND (example_query, AND (player_query, health_query)) in
  let seq = World.query_sequence world query in
  if Sequence.length seq <> 1 then failwith "Failed to get BOTH values";
  let _, result = Sequence.nth_exn seq 0 in
  (match result with
   | true, ((), 22.0) -> ()
   | _ -> failwith "Not the right values for BOTH");
  (* Let's look for things that ARE NOT the player *)
  let query =
    let query = AND (example_query, health_query) in
    NOT { query; condition = player_query }
  in
  let seq = World.query_sequence world query in
  if Sequence.length seq <> 1 then failwith "Failed to get BOTH values";
  (* Now let's do it with a WITH query *)
  let _ =
    let query =
      WITH { query = AND (example_query, health_query); condition = player_query }
    in
    let seq = World.query_sequence world query in
    if Sequence.length seq <> 1 then failwith "Failed to get BOTH values";
    let _, result = Sequence.nth_exn seq 0 in
    match result with
    | true, 22.0 -> ()
    | _ -> failwith "Not the right values for BOTH"
  in
  ()
;;

let test_with_query () =
  let open Component in
  let lookup = Lookup.empty () in
  (* 1 has Example and Health *)
  Lookup.set lookup (module Example) first_id false;
  Lookup.set lookup (module Health) first_id 1.0;
  Lookup.set lookup (module PlayerTag) first_id ();
  (* 2 has just Example *)
  Lookup.set lookup (module Example) second_id true;
  let open Query in
  let example_query = component (module Example) in
  let health_query = component (module Health) in
  let query = AND (example_query, health_query) in
  let player_query = component (module PlayerTag) in
  let with_query = WITH { query; condition = player_query } in
  let should_be_some = query_lookup lookup with_query first_id in
  (match should_be_some with
   | Some (false, 1.0) -> ()
   | _ -> failwith "Should have been some");
  let should_be_none = query_lookup lookup with_query second_id in
  (match should_be_none with
   | None -> ()
   | _ -> failwith "Should have been none");
  ()
;;

(* let test_both_queries () = *)
(*   let open Component in *)
(*   let open Raylib in *)
(*   let world = World.empty () in *)
(*   let lookup = world.lookup in *)
(*   (* 1 is a player with a position *) *)
(*   Lookup.set lookup (module PlayerTag) 1 (); *)
(*   Lookup.set lookup (module Position) 1 (Vector2.create 1.0 1.0); *)
(*   (* 2 = enemy *) *)
(*   Lookup.set lookup (module EnemyTag) 2 (); *)
(*   Lookup.set lookup (module Position) 2 (Vector2.create 2.0 2.0); *)
(*   (* 3 = enemy *) *)
(*   Lookup.set lookup (module EnemyTag) 3 (); *)
(*   Lookup.set lookup (module Position) 3 (Vector2.create 3.0 3.0); *)
(*   let make_query condition = *)
(*     Query.(WITH { query = COMPONENT (module Position); condition = COMPONENT condition }) *)
(*   in *)
(*   let player_query = make_query (module PlayerTag) in *)
(*   let enemy_query = make_query (module EnemyTag) in *)
(*   let player_count = World.to_hash world player_query |> Hashtbl.length in *)
(*   assert (player_count = 1); *)
(*   let enemy_count = World.to_hash world enemy_query |> Hashtbl.length in *)
(*   assert (enemy_count = 2); *)
(*   let player_combinator = World.ITER player_query in *)
(*   let enemy_combinator = World.ITER enemy_query in *)
(*   let both_combinator = World.ZIP (player_combinator, enemy_combinator) in *)
(*   let _ = World.do_the_query world both_combinator in *)
(*   (* let both_query = Query.ZIP (enemy_query, player_query) in *) *)
(*   (* let bothed = World.to_hash world both_query in *) *)

(* some system:
   fun _ (player, enemies) -> ...
*)
()

let test_map_sort () =
  let map = Map.empty (module Int) in
  let map = Map.set map ~key:1 ~data:1 in
  let map = Map.set map ~key:4 ~data:4 in
  let map = Map.set map ~key:13 ~data:13 in
  let map = Map.set map ~key:14 ~data:14 in
  let map = Map.set map ~key:2 ~data:2 in
  let map = Map.set map ~key:8 ~data:8 in
  let map = Map.set map ~key:9 ~data:9 in
  let map = Map.set map ~key:5 ~data:5 in
  let map = Map.set map ~key:6 ~data:6 in
  let map = Map.set map ~key:10 ~data:10 in
  let map = Map.set map ~key:11 ~data:11 in
  let map = Map.set map ~key:18 ~data:18 in
  let map = Map.set map ~key:19 ~data:19 in
  let map = Map.set map ~key:12 ~data:12 in
  let map = Map.set map ~key:15 ~data:15 in
  let map = Map.set map ~key:3 ~data:3 in
  let map = Map.set map ~key:7 ~data:7 in
  let map = Map.set map ~key:16 ~data:16 in
  let map = Map.set map ~key:17 ~data:17 in
  let map = Map.set map ~key:20 ~data:20 in
  let items = ref [] in
  Map.iter map ~f:(fun v -> items := v :: !items);
  Alcotest.(check (list int)) "iter in order" (List.range 1 21) (!items |> List.rev);
  ()
;;

let _ =
  let open Alcotest in
  run
    "Component"
    [ ( "basics"
      , [ test_case "can lookup components" `Quick test_lookup
        ; test_case "can query components" `Quick test_query
        ; test_case "can do not queries" `Quick test_not_query
        ; test_case "can do with queries" `Quick test_with_query
        ] )
    ; ( "iteration"
      , [ test_case "iterate over single query" `Quick test_iter_with_query
        ; test_case "iterate over multi query" `Quick test_iter_with_and_query
        ; test_case "iterate large query" `Quick test_iter_with_and_query_perf
        ] )
      (* ("both", [ test_case "can combine both queries" `Quick test_both_queries ]); *)
    ; "Base.Map", [ test_case "sorted values for map" `Quick test_map_sort ]
    ]
;;

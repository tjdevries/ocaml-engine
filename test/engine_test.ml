open Engine

module Example = Component.ComponentMaker (struct
  type t = bool
  type Component.component += Example of t

  let of_component = function Example b -> b | _ -> failwith "NO"
  let to_component b = Example b
end)

(* TODO: Seems... not good that i have to put this here? but i'm not sure how else to expose... *)
type record = { cool : bool; number : float }

module WithRecord = Component.ComponentMaker (struct
  type t = record
  type Component.component += WithRecord of t

  let of_component = function WithRecord b -> b | _ -> failwith "NO"
  let to_component b = WithRecord b
end)

let test_lookup () =
  let open Component in
  let lookup = Lookup.empty () in
  Lookup.set lookup (module Example) 1 true;
  Lookup.set lookup (module Example) 2 false;
  Lookup.set lookup (module Health) 1 1.0;
  Lookup.set lookup (module WithRecord) 1 { cool = true; number = 1.0 };
  let value = Lookup.retrieve lookup (module Example) 1 in
  (match value with Some true -> () | _ -> failwith "NO");

  let value = Lookup.retrieve lookup (module Health) 1 in
  (match value with Some 1.0 -> () | _ -> failwith "NO");

  let value = Lookup.retrieve lookup (module WithRecord) 1 in
  (match value with
  | Some { cool = true; number = 1.0 } -> ()
  | _ -> failwith "DID NOT MATCH");

  let value = Lookup.retrieve lookup (module Example) 2 in
  (match value with Some false -> () | _ -> failwith "NO");

  let value = Lookup.retrieve lookup (module Example) 3 in
  (match value with None -> () | _ -> failwith "NO");
  ()

let test_query () =
  let open Component in
  let lookup = Lookup.empty () in

  (* Fill with some data *)
  Lookup.set lookup (module Example) 1 true;
  Lookup.set lookup (module Health) 1 1.0;

  Lookup.set lookup (module Example) 2 false;
  Lookup.set lookup (module Health) 2 13.0;

  Lookup.set lookup (module Example) 3 false;

  (* Create the queries *)
  let x = Query.component (module Example) in
  let y = Query.component (module Health) in
  let both = Query.AND (x, y) in

  (*  Do some querying *)
  let result = Query.query_lookup lookup both 1 in
  (match result with
  | Some (true, 1.0) -> ()
  | _ -> failwith "Could not with match this");

  let result = Query.query_lookup lookup both 2 in
  (match result with
  | Some (false, 13.0) -> ()
  | _ -> failwith "Could not with match this");

  (* Confirm that adding the `health *)
  let () =
    (match Query.query_lookup lookup both 3 with
    | None -> ()
    | _ -> failwith "Should not find this");

    Lookup.set lookup (module Health) 3 3.0;

    match Query.query_lookup lookup both 3 with
    | Some (false, 3.0) -> ()
    | _ -> failwith "Should not find this"
  in

  ()

let test_not_query () =
  let open Component in
  let lookup = Lookup.empty () in

  (* 1 has Example and Health *)
  Lookup.set lookup (module Example) 1 true;
  Lookup.set lookup (module Health) 1 1.0;

  (* 2 has just Example *)
  Lookup.set lookup (module Example) 2 true;

  let open Query in
  let example_query = component (module Example) in
  let not_query =
    NOT { query = example_query; condition = component (module Health) }
  in

  let should_be_none = query_lookup lookup not_query 1 in
  (match should_be_none with
  | None -> ()
  | _ -> failwith "Should have been none");

  let should_be_some = query_lookup lookup not_query 2 in
  (match should_be_some with
  | Some true -> ()
  | _ -> failwith "Should have been none");

  ()

let test_with_query () =
  let open Component in
  let lookup = Lookup.empty () in

  (* 1 has Example and Health *)
  Lookup.set lookup (module Example) 1 false;
  Lookup.set lookup (module Health) 1 1.0;
  Lookup.set lookup (module PlayerTag) 1 ();

  (* 2 has just Example *)
  Lookup.set lookup (module Example) 2 true;

  let open Query in
  let example_query = component (module Example) in
  let health_query = component (module Health) in
  let query = AND (example_query, health_query) in
  let player_query = component (module PlayerTag) in
  let with_query = WITH { query; condition = player_query } in

  let should_be_some = query_lookup lookup with_query 1 in
  (match should_be_some with
  | Some (false, 1.0) -> ()
  | _ -> failwith "Should have been some");

  let should_be_none = query_lookup lookup with_query 2 in
  (match should_be_none with
  | None -> ()
  | _ -> failwith "Should have been none");

  ()

let _ =
  let open Alcotest in
  run "Component"
    [
      ( "basics",
        [
          test_case "can lookup components" `Quick test_lookup;
          test_case "can query components" `Quick test_query;
          test_case "can do not queries" `Quick test_not_query;
          test_case "can do with queries" `Quick test_with_query;
        ] );
    ]

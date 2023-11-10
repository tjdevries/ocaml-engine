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
  Lookup.add_component lookup (module Example);
  Lookup.add_component lookup (module Health);
  Lookup.add_component lookup (module WithRecord);
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
  Lookup.add_component lookup (module Example);
  Lookup.add_component lookup (module Health);

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

let _ =
  let open Alcotest in
  run "Component"
    [
      ( "basics",
        [
          test_case "can lookup components" `Quick test_lookup;
          test_case "can query components" `Quick test_query;
        ] );
    ]

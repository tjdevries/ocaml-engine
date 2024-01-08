type ordered_systems = (int * (module System.SYSTEM)) list

let execute_systems world (systems : ordered_systems) =
  List.iter systems ~f:(fun (_, (module Sys)) -> Sys.execute world Sys.resources)
;;

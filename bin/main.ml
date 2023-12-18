(* TODO: Find out if we can abuse algebraics effect & multicore to make this zoom zoom *)
(* TODO: Flyweight pattern (making sure to share textures, meshes, etc) *)
(*          Can use references to all the same object, which i think should work the same *)
(* TODO: instaced rendering -> https://en.wikipedia.org/wiki/Geometry_instancing *)
(* Stats tracking...
    Separate core that listens for events and does all the tracking based on that?
*)

(* let width = 3840 *)
(* let height = 2160 *)
(* let screen_width = 1000 *)

let _ = print_endline "Hello World@."
let _ = Engine.Game.main ()
let _ = print_endline "Goodbye World@."

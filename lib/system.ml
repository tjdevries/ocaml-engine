(* TODO: how to use systems to do this... *)

(* TODO: How do we make Archetypes...
    Archetypes are combinations of components that (sometimes common ones, sometimes always) hold
    data together which let's you go really zoom zoom when doing system stuff.

    You just run through an array and do stuff :)

   Might not need archetypes if everything else is fast enough
*)

type 'a system = { query : 'a Query.t; f : 'a -> unit }

let run (s : 'a system) (a : 'a) : unit = s.f a

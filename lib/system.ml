module type SYSTEM = sig
  type resources

  val resources : resources
  val execute : World.t -> resources -> unit
end

let make : type a. a -> (World.t -> a -> unit) -> (module SYSTEM) =
  fun resources execute ->
  (module struct
    type resources = a

    let execute = execute
    let resources = resources
  end)
;;

let make_pure execute = make () (fun world _ -> execute world)
let foreach query f = make () (fun world _ -> World.iter_query world query f)

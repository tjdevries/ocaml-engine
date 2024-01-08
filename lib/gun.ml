open Component

type gun =
  { mutable ammo : int
  ; capacity : int
  ; damage : int
  }

module Gun = Component.ComponentMaker (struct
    type t = gun
    type component += Gun of t

    let of_component = function
      | Gun t -> t
      | _ -> failwith "bad value"
    ;;

    let to_component t = Gun t
  end)

(* module PlayerAttached = Component.ComponentMaker (struct *)
(*     type t =  *)
(*     type component += Gun of t *)
(*     let of_component = function *)
(*       | Gun t -> t *)
(*       | _ -> failwith "bad value" *)
(*     ;; *)
(*     let to_component t = Gun t *)
(*   end) *)

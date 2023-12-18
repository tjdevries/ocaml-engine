```ocaml

        (* Closest mob *)
        let () =
          match
            MobStore.fold mobs ~init:None ~f:(fun acc mob ->
                match acc with
                | Some acc ->
                    let center = rectangle_to_vec2 mob.rect in
                    let distance = Vector2.distance player_center center in
                    let acc_center = rectangle_to_vec2 Mob.(acc.rect) in
                    let acc_distance =
                      Vector2.distance player_center acc_center
                    in
                    if Float.(distance < acc_distance) then Some mob
                    else Some acc
                | None -> Some mob)
          with
          | Some closest ->
              let closest_center = rectangle_to_vec2 closest.rect in

              (* draw_line_ex player_center closest_center 10.0 Color.red; *)
              let direction =
                Vector2.normalize
                  (Vector2.subtract closest_center player_center)
              in

              if Float.(Stdlib.mod_float (of_int tick) 30.0 = 0.0) then (
                let rect = RectUtils.copy player in
                Rectangle.set_height rect 15.0;
                Rectangle.set_width rect 15.0;
                bullets :=
                  Bullet.create ~rect ~damage:6.0 ~speed:8.0 ~direction
                    ~ticks_remaining:600 ~pierce:15
                  :: !bullets)
```


```ocaml

      Camera2D.set_target camera
        (Vector2.create
           (Rectangle.x player +. 20.0)
           (Rectangle.y player +. 20.0));

      (* Update mob positions *)
      let player_center = rectangle_to_vec2 player in
      MobStore.iter mobs ~f:(fun mob ->
          let mob_center = rectangle_to_vec2 mob.rect in
          let direction = Vector2.subtract player_center mob_center in
          let direction = Vector2.normalize direction in
          Rectangle.set_x mob.rect
            (Rectangle.x mob.rect +. (Vector2.x direction *. mob.speed));
          Rectangle.set_y mob.rect
            (Rectangle.y mob.rect +. (Vector2.y direction *. mob.speed)));

      (* Sword.update sword; *)
      MobStore.filter_inplace mobs ~f:(fun mob ->
          match
            List.find !bullets ~f:(fun bullet -> Bullet.can_hit bullet mob)
          with
          | Some bullet -> Bullet.hit_mob bullet mob
          | None -> true);
```


```ocaml

(* module Bullet = struct *)
(*   type t = { *)
(*     rect : Rectangle.t; *)
(*     damage : float; *)
(*     speed : float; *)
(*     direction : Vector2.t; *)
(*     mutable ticks_remaining : int; *)
(*     mutable pierce : int; *)
(*     mobs_hit : int Array.t; *)
(*   } *)
(*   let create ~rect ~damage ~speed ~direction ~ticks_remaining ~pierce = *)
(*     let mobs_hit = Array.create ~len:pierce 0 in *)
(*     { rect; damage; speed; direction; ticks_remaining; pierce; mobs_hit } *)
(*   let can_hit t (mob : Mob.t) = *)
(*     t.pierce > 0 *)
(*     && check_collision_recs t.rect mob.rect *)
(*     && not (Array.mem t.mobs_hit mob.id ~equal:Int.equal) *)
(*   let hit_mob t (mob : Mob.t) = *)
(*     t.pierce <- t.pierce - 1; *)
(*     Array.set t.mobs_hit t.pierce mob.id; *)
(*     Mob.take_damage mob t.damage *)
(*   let draw t = *)
(*     (* draw_texture_ex t.texture (rectangle_to_vec2 t.rect) 1.0 0.05 Color.white *) *)
(*     draw_rectangle_rec t.rect Color.white *)
(*   let update t = *)
(*     if t.ticks_remaining <= 0 || t.pierce <= 0 then false *)
(*     else *)
(*       let _ = Vector2.lerp in *)
(*       Rectangle.set_x t.rect *)
(*         (Rectangle.x t.rect +. (Vector2.x t.direction *. t.speed)); *)
(*       Rectangle.set_y t.rect *)
(*         (Rectangle.y t.rect +. (Vector2.y t.direction *. t.speed)); *)
(*       t.ticks_remaining <- t.ticks_remaining - 1; *)
(*       true *)
(* end *)
```

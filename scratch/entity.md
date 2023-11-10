

What is an Entity?

So, we can't go full ECS cause that's too hard.

Instead, we can make kind of ECS by using modules and functors, I think.
- Potentially could use objects... but I don't think we need to

```ocaml
module type Weapon = sig
  type t

  val position ?

  val collides : t -> (w: Weapon) -> bool
end
```

- resources
- components
- entities
- systems
- queries

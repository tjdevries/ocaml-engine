# Installation

```
$ sudo apt install build-essential git

$ sudo apt install libasound2-dev libx11-dev libxrandr-dev libxi-dev libgl1-mesa-dev libglu1-mesa-dev libxcursor-dev libxinerama-dev

# not sure you need to do this?
$ opam depext raylib

```

# TODO

- Bundles
- Commands
- Resources
- Events
- Time?

- Scheduling / Stagin
  - https://bevy-cheatbook.github.io/programming/schedules.html

## Potential Features

- Dash (iframes?)
- Teleport?
- Auto-aim, Auto-shoot
- Defensive Skills? (e.g. shield)

- Questions:
  - Should most abilities be activated or passively applied?





- could play w/ light and vision a bit
- could play w/ orientation

## Programming features

- Scouting unit
- Communication unit
- Summon unit
  - Pathfinding, Enemy location, status, speed, type, range, etc.
  - Some units/upgrade do more damage each consecutive hit they do
- Melee unit
- Tanky unit
  - searching for units that do damage and getting between them
- Ranged unit
- Buff-teammates-unit
- Debuff-enemies-unit

- Handle getting slowed / hasted?
- Overheating (too many attacks)
- Friendly fire
- walls / vision problems
- definitely make it possible to get screwed by off-by-ones
- Auras (and calculating to stay in an aura!)

What are some common problems in these games?
- overkill
- ranged guys being annoying
- big bosses crushing you
- ppl doing damage to you?!




## Rogue-like round

- Vim mode (move with hjkl)
- Fully automatic mode?
- Instead of survival mode, you could have speedrun mode

Main Question:
- Should we introduce gacha style gambling to unlock popular tech twitch streamers w/
  different skill levels and star counts?!

- Programmers:
  - piq: no special skills, doesn't do anything, just keeps talking about haskell
  - melkey: jacked out of his mind, punches enemies, code runs fast and good (golang)
      - sometimes panics....

- piq, a "programmer"
  - Each programmer has some different skills and/or bonuses
  - Also some simple attack

  - throughout the round, you can pick up new robots
    - you can write spells for the robots outside or inside of the game loop
    - the spells are shared between programmers

    - as you level up, you get upgrades for:
      - your spells, programmer, robot, calculations, effects, etc


mobs that have a certain number of blocked:
- hits, regardless of damage
- damage, regardless of hits
- time?
- magic?


armor % reduction
armot base reduction

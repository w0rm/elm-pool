# elm-pool

Physically simulated pool game. Work in progress.

## User Interface

- Common controls

  1. Click and drag anywhere to rotate the camera around the focal point ✅
  2. Zoom into the focal point with the mousewheel ✅

- Placing the cue ball in hand

  1. The focal point is at the center of the table ✅
  2. Hover over the table to choose where to place the cue ball ✅
  3. The cue ball is rendered in gray when it overlaps with other objects ✅
  4. Click to place the cue ball if it is not overlapping with other objects ✅

- Aiming and shooting

  1. The focal point is at the cue ball ✅
  2. The camera angle defines the azimuth of the cue ✅
  3. Click and hold on the cue ball to set the hit point, move cursor while pressed to set the cue elevation ✅
  4. The cue is rendered in gray if it overlaps with other objects ✅
  5. Press and hold the "Space" button to set the force ✅
  6. Release the "Space" button to shoot ✅

- Simulating

  1. The focal point is where it was before ✅
  2. The camera is zoomed out to show the table (unless interrupted with the mousewheel) ✅

## Game Rules

The game format is two (2) player 8-Ball. It is a simplified version of [WPA 8-Ball rules](https://wpapool.com/rules-of-play/#eight-ball). The goal is to successfully pocket all seven (7) of one group (solids or stripes) without pocketing the cue ball, then pocket the 8-ball.

- Getting started

  1. Rack the balls ✅
  2. Place the cue ball behind the head string ✅

- Target group

  1. View the current target group ✅
  2. The table is considered "open" until someone has pocketed a solid or stripe without scratching ✅
  3. When the table is open, pocketing balls in only one of the solids or stripes group without scratching sets the target for that player ✅

- During play

  1. View the current player ✅
  2. Add shot with shot events ✅
  3. Place ball in hand ✅
  4. Check if shot is legal, that is, if the player has hit their target ball with the cue before any other, and some ball has hit a rail or went into a pocket ✅
  5. On scratch, require the next player to place the cue ball anywhere on the table (ball-in-hand) ✅
  6. If a player does not pocket one of their balls or scratches, the current player switches ✅
  7. Ensure balls are pocketed only once or send an error
  8. Support "spotted" balls when the numbered balls fall off the table

- Winning the game

  1. If a player has pocketed all of their target balls and makes the 8-ball without scratching, they win ✅
  2. When a player has pocketed all of their target balls and makes the 8-ball but scratches, they lose ✅
  3. If a player pockets the 8-ball before all of their target balls, they lose ✅

## Developing with nix flakes

We are using the nix package manager, that can be installed following [these instructions](https://nixos.org/download.html). To activate the flakes feature, include `experimental-features = nix-command flakes` in `~/.config/nix/nix.conf` (create this file if it doesn't exist).

Then, run `nix develop` from the project root. This will give you development environment with `elm`, so you can run:

```bash
elm reactor
```

Open http://localhost:8000/src/Main.elm.

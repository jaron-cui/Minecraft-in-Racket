TODO:
- Block-breaking
- Auto terrain-generation
- Mobs
- Inventory / Submenus
- Crafting
- Additional physics (water)

Known Bugs:
- You can walk into solid blocks if you walk diagonally at a corner,
  because collision checking currently checks motion axes individually,
  so only blocks orthogonally adjacent to the player position are checked
  for solidity.
  (ex. If you are moving along the x and y-axes simultaneously, then even
  if all blocks on your x axis are non-solid, your concurrent y-motion
  might cause you to end up inside a block even though your check did not
  find solid blocks on the previous tick)
  (EASY FIX ANTICIPATED)
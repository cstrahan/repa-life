Conway's Life in Haskell using repa and OpenGL
==========

About
----------

This is an implementation of [Conway's Life][life] in [Haskell][] that
uses [repa][] for the simulation and OpenGL for the view. I detailed
the implementation in [this blog post][post]. There is an
[earlier implementation using flat vectors and Gloss][old version].

Use
----------

This package is cabalized, so build the executable with the following commands

~~~~
cabal configure
cabal build
~~~~

Usage: `repa-life <grid size> [generations]`

grid size
: This controls how many cells are in the Life grid. If grid size is
  N, the grid will be NxN cells.

generations
: This optional argument will limit the length of the simulation. If
  this is not present, the program will run until it is manually
  terminated.

[life]: http://en.wikipedia.org/wiki/Conway's_Game_of_Life
[Haskell]: http://haskell.org/
[repa]: http://hackage.haskell.org/package/repa
[post]: http://www.tapdancinggoats.com/haskell-life-repa.htm
[old version]: http://www.tapdancinggoats.com/haskell-life.htm

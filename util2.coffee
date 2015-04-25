{Map2, Set2, Map3, Set3} = require './collections2'
log = require './log'

chars =
  positive: '+'
  negative: '-'
  nothing: ' '
  thinsolid: 'x'
  shuttle: 'S'
  thinshuttle: 's'
  bridge: 'b'

UP=0; RIGHT=1; DOWN=2; LEFT=3
DN =
  0: 'UP'
  1: 'RIGHT'
  2: 'DOWN'
  3: 'LEFT'

DIRS = exports.DIRS = [
  {dx:0, dy:-1}
  {dx:1, dy:0}
  {dx:0, dy:1}
  {dx:-1, dy:0}
]

parseXY = exports.parseXY = (k) ->
  [x,y] = k.split ','
  {x:x|0, y:y|0}

# Flood fill through the grid from a cell
exports.fill = (initialX, initialY, f) ->
  visited = new Set2 [[initialX, initialY]]
  explore = [initialX, initialY]

  # Consider a cell
  hmm = (x, y) ->
    if !visited.has x, y
      visited.add x, y
      explore.push x; explore.push y

  while explore.length > 0
    x = explore.shift(); y = explore.shift()
    
    if f x, y, hmm
      hmm x+1, y
      hmm x-1, y
      hmm x, y+1
      hmm x, y-1

  return



oppositeDir = (dir) -> (dir+2) % 4

cellAt = (grid, x, y, dir) ->
  v = grid.get x, y
  #log 'cellat', x, y, DN[dir], v
  switch v
    # Shuttle in this list because there's no guarantee that the shuttle is
    # still here later.
    when 'nothing', 'thinsolid', 'thinshuttle', 'shuttle'
      [x, y]
    when 'bridge'
      [x, y, if dir in [UP, DOWN] then 0 else 1]
    when 'negative', 'positive'
      [x, y, dir]
    else
      null

cellOpposite = (grid, x, y, dir) ->
  {dx, dy} = DIRS[dir]
  #log 'cellOpposite', x, y, DN[dir], x+dx, y+dy
  cellAt grid, x+dx, y+dy, oppositeDir(dir)

exports.cellMax = (v) ->
  switch v
    when 'positive', 'negative' then 4
    when 'bridge' then 2
    when null then 0
    else 1

# Flood fill through the grid from a cell
exports.fillCells = (grid, initialX, initialY, initialC, f) ->
  visited = new Set3 [[initialX, initialY, initialC]]
  explore = [initialX, initialY, initialC]

  # Consider a cell
  hmm = (x, y, c) ->
    if !visited.has x, y, c
      visited.add x, y, c
      explore.push x; explore.push y; explore.push c

  while explore.length > 0
    x = explore.shift(); y = explore.shift(); c = explore.shift()
    
    v = grid.get x, y
    continue unless v # No walls.
    if f x, y, c, v, hmm
      # Hmm for each potentially adjacent cell.
      dirs = switch v
        when 'bridge'
          if c is 0 then [UP, DOWN] else [LEFT, RIGHT]
        when 'positive', 'negative'
          [c]
        when 'nothing', 'thinsolid', 'thinshuttle', 'shuttle'
          [UP, RIGHT, DOWN, LEFT]


      for d in dirs
        cell = cellOpposite grid, x, y, d
        if cell
          hmm cell[0], cell[1], cell[2]||0

  return


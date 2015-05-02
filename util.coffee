{Map2, Set2, Map3, Set3} = require './collections2'
log = require './log'
assert = require 'assert'

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



oppositeDir = exports.oppositeDir = (dir) -> (dir+2) % 4

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

exports.cellMax = (v) ->
  switch v
    when 'positive', 'negative' then 4
    when 'bridge' then 2
    when null, undefined then 0
    else 1

connectedCells = exports.connectedCells = (grid, x, y, c) ->
  # Returns a list of cells (list of lists)
  v = grid.get x, y
  dirs = switch v
    when 'bridge'
      if c is 0 then [UP, DOWN] else [LEFT, RIGHT]
    when 'positive', 'negative'
      [c]
    when 'nothing', 'thinsolid', 'thinshuttle', 'shuttle'
      [UP, RIGHT, DOWN, LEFT]
    else
      [] # Nothing is connected to nothin'.

  cells = []
  for dir in dirs
    {dx,dy} = DIRS[dir]
    cell = cellAt grid, x+dx, y+dy, oppositeDir(dir)
    cells.push cell if cell
  cells


# Flood fill through the grid from a cell
exports.fill3 = (a0, b0, c0, f) ->
  visited = new Set3
  visited.add a0, b0, c0
  explore = [a0, b0, c0]

  # Consider a cell
  hmm = (x, y, c) ->
    if !visited.has x, y, c
      visited.add x, y, c
      explore.push x; explore.push y; explore.push c

  while explore.length > 0
    a = explore.shift(); b = explore.shift(); c = explore.shift()
    
    f a, b, c, hmm

  return


exports.uniqueShuttlesInStates = (states) ->
  shuttles = []
  marked = new WeakSet
  states.forEach ({shuttle}) ->
    return if marked.has shuttle
    marked.add shuttle
    shuttles.push shuttle
  shuttles.sort (a, b) -> a.id - b.id

  shuttles

exports.setToArray = (set) ->
  arr = []
  set.forEach (x) -> arr.push(x)
  arr

exports.ShuttleStateMap = class ShuttleStateMap
  constructor: (shuttleSet) ->
    # First, find which shuttles are in play here. Its important that we iterate
    # through the shuttles in a stable order, so we'll throw everything into
    # lists.
    @shuttles = []

    shuttleSet.forEach (s) =>
      assert s.used
      @shuttles.push s

    @values = undefined

  each = (list, depth, fn) ->
    if depth is 0
      fn list if list?
      return
    depth--
    each item, depth, fn for item in list when item

  # Is the map defined for some subset of the states in the grid?
  isDefinedFor: (currentStates) ->
    for s in @shuttles
      return no unless currentStates.has s
    return yes

  get: (currentStates) ->
    container = @values
    for s in @shuttles
      return unless container

      state = currentStates.get s
      assert state # The state set doesn't define a value!
      container = container[state.id]

    return container

  set: (currentStates, v) ->
    return @values = v if @shuttles.length is 0 # optimization

    key = 'values'
    container = this

    for s in @shuttles
      state = currentStates.get s
      throw Error 'ShuttleStateMap.set on an unbound set' unless state

      if !container[key]
        container = container[key] = []
      else
        container = container[key]
      key = state.id

    container[key] = v

  delete: (currentStates) -> @set currentStates, undefined

  # I can make a normal forEach too, but this is simpler and faster.
  forEachValue: (fn) ->
    each @values, @shuttles.length, fn

exports.fillGraph = (initialNode, f) ->
  visited = new Set
  explore = []

  hmm = (node) ->
    if !visited.has node
      visited.add node
      explore.push node
  hmm initialNode

  while explore.length > 0
    node = explore.shift()
    f node, hmm

  return


exports.printCustomGrid = printCustomGrid = ({top, left, bottom, right}, getFn, stream = process.stdout) ->
  top ||= 0; left ||= 0

  # Header
  stream.write '+ '
  for x in [left..right]
    stream.write "#{x%10}"
  stream.write '\n'

  for y in [top..bottom]
    stream.write "#{y%10} "
    for x in [left..right]
      v = getFn(x, y)
      stream.write chars[v] || (if v? then ("#{v}")[0] else ';')
    stream.write '\n'

  stream.write '+ '
  for x in [left..right]
    stream.write "#{x%10}"
  stream.write '\n'

exports.gridExtents = (grid) ->
  # calculate the extents
  top = left = bottom = right = null

  grid.forEach (x, y, v) ->
    left = x if left is null || x < left
    right = x if right is null || x > right
    top = y if top is null || y < top
    bottom = y if bottom is null || y > bottom

  {top, left, bottom, right}

jsonExtents = (grid) ->
  # calculate the extents
  top = left = bottom = right = null

  for k, v of grid
    {x,y} = parseXY k
    left = x if left is null || x < left
    right = x if right is null || x > right
    top = y if top is null || y < top
    bottom = y if bottom is null || y > bottom

  {top, left, bottom, right}

exports.printJSONGrid = (grid, stream = process.stdout) ->
  extents = jsonExtents grid
  printCustomGrid extents, ((x, y) -> grid[[x,y]]), stream

exports.printGrid = (extents, grid, stream = process.stdout) ->
  printCustomGrid extents, ((x, y) -> grid.get x, y), stream


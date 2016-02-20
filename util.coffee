{Map2, Set2, Map3, Set3} = require './collections2'
log = require './log'
assert = require 'assert'
chalk = require 'chalk'
do -> # It might not actually be worth doing this. Turns out chalk is really small!
  if !chalk.bgGreen
    chalk = (x) -> x
    for fn in ['bgGreen','bgRed','bgWhite','bgBlue','blue','yellow','grey','magenta']
      chalk[fn] = chalk
chars =
  positive: chalk.bgGreen '+'
  negative: chalk.bgRed '-'
  nothing: chalk.bgWhite ' '
  thinsolid: chalk.bgWhite.grey 'x'
  shuttle: chalk.magenta 'S'
  thinshuttle: chalk.magenta.bgWhite 's'
  bridge: chalk.bgBlue 'B'
  thinbridge: chalk.blue 'b'
  ribbon: chalk.yellow 'r'
  ribbonbridge: chalk.yellow.bgBlue 'r'

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


NUMINS = exports.NUMINS = 16
insNum = exports.insNum = do ->
  map = {}
  map["ins#{i}"] = i-1 for i in [1..16]
  (v) -> map[v] ? -1


SHUTTLE     = 0x80
THINSHUTTLE = 0x40

shuttleStr = exports.shuttleStr = (v) ->
  if (v & SHUTTLE)
    'shuttle'
  else if (v & THINSHUTTLE)
    'thinshuttle'
  else
    null

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



oppositeDir = exports.oppositeDir = (dir) -> (dir+2) % 4

###

inum, inum2, result
0, 0, normal
0, x, normal

x, y, null
x, x, [x,y,0]
x, 0, [x,y,0]

###

exports.cellMax = (v) ->
  switch v
    when 'positive', 'negative' then 4
    when 'bridge' then 2
    when 'ribbon' then NUMINS
    when 'ribbonbridge' then NUMINS*2
    when null, undefined then 0
    else 1

insLevelOf = (v) ->
  if v in ['ribbon', 'ribbonbridge']
    0b10
  else if insNum(v) != -1
    0b11
  else
    0b01

cellAt = (grid, x, y, dir, insLevel, inum2) ->
  # console.log 'cellAt', arguments
  v = grid.get x, y
  return null unless insLevel & insLevelOf(v)
  if (inum = insNum v) != -1
    if inum2 in [-1, inum] then [x, y, 0] else null
  else switch v
    when 'ribbon'
      assert inum2 != -1
      [x, y, inum2]
    when 'ribbonbridge'
      [x, y, if dir in [UP, DOWN] then inum2 else inum2 + NUMINS]
    when 'nothing', 'thinsolid'
      [x, y, 0]
    when 'bridge'
      [x, y, if dir in [UP, DOWN] then 0 else 1]
    when 'negative', 'positive'
      [x, y, dir]
    else
      null

connectedCells = (grid, x, y, c) ->
  # Returns a list of cells (list of lists)
  v = grid.get x, y

  # -1 if not insulated.
  inum = insNum v
  insLevel = 0b01
  dirs = if inum != -1
    insLevel = 0b11
    [UP, RIGHT, DOWN, LEFT]
  else
    if v in ['ribbon', 'ribbonbridge']
      inum = c % NUMINS
      insLevel = 0b10
    switch v
      when 'nothing', 'thinsolid', 'ribbon'
        [UP, RIGHT, DOWN, LEFT]
      when 'bridge'
        if c is 0 then [UP, DOWN] else [LEFT, RIGHT]
      when 'ribbonbridge'
        if c < NUMINS then [UP, DOWN] else [LEFT, RIGHT]
      when 'positive', 'negative' then [c]
      else
        [] # Nothing is connected to nothin'.

  cells = []

  for dir in dirs
    {dx,dy} = DIRS[dir]
    cell = cellAt grid, x+dx, y+dy, oppositeDir(dir), insLevel, inum
    cells.push cell if cell

  # console.log 'connectedCells', x, y, c, v, inum, insLevel, cells

  cells

exports.connectedCells = (grid, x, y, c) ->
  cells = connectedCells grid, x, y, c
  # connectedCells must be reflexive.
  # for [x1, y1, c1] in cells
  #   set = new Set3 connectedCells grid, x1, y1, c1
  #   assert set.has x, y, c
  return cells



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

  header = chalk.bold

  # Header
  stream.write header '+ '
  for x in [left..right]
    stream.write header "#{x%10}"
  stream.write '\n'

  for y in [top..bottom]
    stream.write header "#{y%10} "
    for x in [left..right]
      v = getFn(x, y)
      v = shuttleStr v if typeof v is 'number'
      stream.write chars[v] || (if v? then ("#{v}")[0] else ';')
    stream.write '\n'

  stream.write header '+ '
  for x in [left..right]
    stream.write header "#{x%10}"
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

  scan = (g) ->
    for k, v of g
      {x,y} = parseXY k
      left = x if left is null || x < left
      right = x if right is null || x > right
      top = y if top is null || y < top
      bottom = y if bottom is null || y > bottom

  if grid.base
    scan grid.base
    scan grid.shuttles
  else
    scan grid

  {top, left, bottom, right}

exports.printJSONGrid = (grid, stream = process.stdout) ->
  extents = jsonExtents grid
  fn = if grid.base
    (x, y) -> grid.shuttles[[x,y]] || grid.base[[x,y]]
  else
    (x, y) -> grid[[x,y]]
  printCustomGrid extents, fn, stream

exports.printGrid = (extents, grid, stream = process.stdout) ->
  printCustomGrid extents, ((x, y) -> grid.get x, y), stream


# Convert a string back to a bp fragment or grid.
#
# setCell is called with (x, y, baseV, shuttleV)
exports.deserialize = deserialize = (data, rebase, setCell) ->
  data = JSON.parse data if typeof data is 'string'

  maxx = maxy = -Infinity
  if rebase
    if data.tw?
      minx = miny = 0
      maxx = data.tw; maxy = data.th
    else
      minx = miny = Infinity
      for k, v of data.base ? data
        {x,y} = parseXY k
        minx = x if x < minx; miny = y if y < miny
        maxx = x if x > maxx; maxy = y if y > maxy
      # Add a 1 tile border around it.
      minx--; miny--
      maxx+=2; maxy+=2
  else
    minx = miny = 0

  if data.base
    #console.log 'Loading from new style data'
    for k, v of data.base
      {x,y} = parseXY k
      v = 'bridge' if v is 'thinbridge'
      setCell x - minx, y - miny, v, data.shuttles[k]

    #if rawGrid.modules then for component in rawGrid.modules
  else
    console.log 'Loading from old style data'
    for k, v of data when k not in ['tw', 'th']
      {x,y} = parseXY k
      x -= minx; y -= miny
      if v in ['shuttle', 'thinshuttle']
        setCell x, y, 'nothing', v
      else
        setCell x, y, v, null

  return if rebase then {tw:maxx-minx, th:maxy-miny}

exports.deserializeRegion = (data) ->
  selection =
    base: new Map2
    shuttles: new Map2
  {tw, th} = deserialize data, yes, (x, y, bv, sv) =>
    selection.base.set x, y, bv
    selection.shuttles.set x, y, sv if sv?
  selection.tw = tw; selection.th = th
  return selection

# This jit is designed as an ecosystem of signaling parts.


Watcher = require './watch2'
{Map2, Set2} = require './collections2'
{parseXY, fill} = util = require './util2'
log = require './log'
assert = require 'assert'

makeId = do ->
  nextId = 1
  -> nextId++


DIRS = [
  {dx:0, dy:-1}
  {dx:1, dy:0}
  {dx:0, dy:1}
  {dx:-1, dy:0}
]

letsShuttleThrough = (v) -> v in ['shuttle', 'thinshuttle', 'nothing']


Grid = (rawGrid) ->
  grid = new Map2
  watch = new Watcher (fn) ->
    grid.forEach (x, y, v) ->
      fn x, y, null, v

  for k, v of rawGrid when k not in ['tw', 'th']
    {x,y} = parseXY k
    grid.set x, y, v

  watch: watch
  get: (x, y) -> grid.get x, y
  set: (x, y, v) ->
    oldV = grid.get x, y
    if v != oldV
      grid.set x, y, v
      watch.signal x, y, oldV, v
  forEach: (fn) -> grid.forEach fn


GridBuffer = (match, grid) ->
  # A set of all the cells which are shuttle / thinshuttle and haven't been
  # processed.
  buffer = new Map2

  watch = new Watcher

  grid.watch.forward (x, y, oldv, v) ->
    if oldv in match
      if buffer.has x, y
        # The consumer doesn't know about this value yet. Just remove it from
        # the buffer.
        buffer.delete x, y
      else
        # Its been consumed. Signal - but only after we set the buffer to the
        # new value (if needed).
        signal = yes

    if v in match
      buffer.set x, y, v

    if signal
      watch.signal x, y

  watch: watch
  buffer: buffer
  pump: (x, y) ->
    v = buffer.get x, y
    if v
      buffer.delete x, y
    return v

Shuttles = (grid) ->
  shuttleBuffer = GridBuffer ['shuttle', 'thinshuttle'], grid

  shuttles = new Set
  shuttleGrid = new Map2 # x,y -> shuttle.
  watch = new Watcher

  shuttleBuffer.watch.on (x, y) ->
    s = shuttleGrid.get x, y
    if shuttles.delete s
      log 'Destroyed shuttle at', x, y, s.id
      assert s.used
      s.used = no
      # Put the points we ate back in the buffer.
      s.points.forEach (x2, y2, v) ->
        shuttleBuffer.buffer.set x2, y2, v if x2 != x || y2 != y

      watch.signal s

  makeShuttle = (x, y) ->
    #log 'makeShuttle at', x, y
    #v = shuttleBuffer.peek x, y
    assert shuttleBuffer.buffer.get(x, y) in ['shuttle', 'thinshuttle']

    s =
      id: "s#{makeId()}"
      used: yes
      size: 0 # For debugging
      points: new Map2

    shuttles.add s

    fill x, y, (x, y) =>
      v = shuttleBuffer.pump x, y
      return no unless v

      shuttleGrid.set x, y, s

      s.size++
      s.points.set x, y, v
      return yes

    assert s.size

    log 'added shuttle', s
    return s

  watch: watch

  forEach: (fn) ->
    shuttleBuffer.buffer.forEach (x, y, v) ->
      makeShuttle x, y

    shuttles.forEach fn

  get: (x, y) ->
    if shuttleBuffer.buffer.get x, y
      makeShuttle x, y

    s = shuttleGrid.get x, y
    return s if s?.used

  collapse: (s) ->
    # When any cells are edited along a shuttle's path, we should move the
    # shuttle into its current position (its current state) in the grid. This
    # will force the shuttle to be regenerated (along with its states, starting
    # with the current state).
    throw Error 'not implemented'

ShuttleStates = (grid, shuttles) ->
  # The set of shuttle states. A shuttle's state list starts off with just one
  # entry (the shuttle's starting state). States are added when the shuttle
  # moves
  shuttleStates = new Map # shuttle -> [states]

  # Set of states which fill a given grid cell
  fillGrid = new Map2 -> new Set # Map from (x,y) -> set of states

  # This maps x,y -> a set of shuttles which sometimes occupy this region
  # (including in invalid states)
  # Its different from fillList in three ways:
  #  1. It maps to the shuttle, not the shuttle state
  #  2. It includes any shuttle which occupies a grid cell using thinshuttle
  #  3. It includes invalid states
  #
  # Its used to find & kill shuttles when the grid changes.
  stateGrid = new Map2 -> new Set

  watch = new Watcher

  shuttles.watch.on (s) -> deleteStatesForShuttle s

  deleteStatesForShuttle = (shuttle) ->
    states = shuttleStates.get shuttle
    return unless states

    shuttleStates.delete shuttle

    states.forEach (state) ->
      shuttle.points.forEach (x, y, v) ->
        x += state.dx; y += state.dy

        set = stateGrid.get x, y
        set.delete shuttle

        if state.valid and v is 'shuttle'
          set = fillList.get x, y
          set.delete state

    watch.signal shuttle, states

  canShuttleFitAt = (shuttle, dx, dy) ->
    shuttle.points.forEach (x, y) =>
      # The grid cell is un-enterable.
      if !letsShuttleThrough grid.get(x+dx, y+dy)
        return no
    return yes

  createStateAt = (shuttle, dx, dy) ->
    state =
      dx: dx
      dy: dy
      valid: @canShuttleFitAt shuttle, dx, dy
      successor: [-1, -1, -1, -1]
      shuttle: shuttle

    states = shuttleStates.get shuttle
    if states
      states.push state
    else
      @shuttleStates.set shuttle, [state]

    # Populate the fill list & state grid.
    shuttle.points.forEach (x, y, v) ->
      assert v in ['shuttle', 'thinshuttle']
      x += state.dx; y += state.dy

      log "adding #{v} to fill / state list at", x, y

      stateGrid.get(x, y).add shuttle

      if v is 'shuttle' and state.valid
        fillList.get(x, y).add state

    log 'created new state', state

    state


  watch: watch
  forEach: (s) ->



Jit = (rawGrid) ->
  grid = Grid rawGrid
  #engineBuffer = GridBuffer ['positive', 'negative'], grid

  shuttles = Shuttles grid
  shuttleStates = ShuttleStates grid, shuttles

  shuttles.forEach (s) ->
    log 'shuttle', s

  grid.set 3, 0, null

  shuttles.forEach (s) ->
    log 's2', s




 
parseFile = exports.parseFile = (filename, opts) ->
  torture =
    if filename in ['-t', 'torture']
      filename = 'simple.json'
      yes
    else
      no

  fs = require 'fs'
  data = JSON.parse fs.readFileSync(filename, 'utf8').split('\n')[0]
  # Mmmm, resiliancy.
  delete data.tw
  delete data.th
  jit = new Jit data, opts

  #jit.torture() if torture
  


if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename



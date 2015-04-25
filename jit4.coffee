# This jit is designed as an ecosystem of signaling parts.


Watcher = require './watch2'
{Map2, Set2} = require './collections2'
{parseXY, fill} = util = require './util2'
log = require './log'
assert = require 'assert'

makeId = do ->
  nextId = 1
  -> nextId++

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
      s.points.forEach (x2, y2, v) ->
        shuttleBuffer.buffer.set x2, y2, v if x2 != x || y2 != y

      watch.signal s

      # ??
      #s.points.forEach (x, y) ->
      #  shuttleGrid.delete x, y

  makeShuttle = (x, y) ->
    log 'makeShuttle at', x, y
    #v = shuttleBuffer.peek x, y
    #return unless v in ['shuttle', 'thinshuttle']
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


  forEach: (fn) ->
    shuttleBuffer.buffer.forEach (x, y, v) ->
      makeShuttle x, y

    shuttles.forEach fn

  get: (x, y) ->
    if shuttleBuffer.buffer.get x, y
      makeShuttle x, y

    s = shuttleGrid.get x, y
    return s if s?.used






Jit = (rawGrid) ->
  grid = Grid rawGrid
  #engineBuffer = GridBuffer ['positive', 'negative'], grid

  shuttles = Shuttles grid

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



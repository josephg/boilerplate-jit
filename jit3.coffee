{parseXY, fill} = util = require './util'
{Watcher, txn} = require './watch'
log = require './log'
assert = require 'assert'
mersenne = require 'mersenne'
mersenne.seed 1234

UP=0; RIGHT=1; DOWN=2; LEFT=3
DN =
  0: 'UP'
  1: 'RIGHT'
  2: 'DOWN'
  3: 'LEFT'

DIRS = [
  {dx:0, dy:-1}
  {dx:1, dy:0}
  {dx:0, dy:1}
  {dx:-1, dy:0}
]

# up, right, down, left
edges = [
  {ex:0,ey:0,isTop:true}
  {ex:1,ey:0,isTop:false}
  {ex:0,ey:1,isTop:true}
  {ex:0,ey:0,isTop:false}
]



oppositeDir = (dir) -> (dir+2) % 4

pressureOf = (v) -> if v is 'positive' then 1 else -1

abs = (x) -> if x < 0 then -x else x


class Grid
  constructor: ->
    @rows = new Map
    @watch = new Watcher (fn) => @forEach fn

  get: (x, y) ->
    row = @rows.get x
    return row.get y if row

  set: (x, y, v) ->
    oldValue = @get x, y
    row = @rows.get x
    if !row
      row = new Map
      @rows.set x, row

    if v
      row.set y, v
    else
      row.delete y

    if oldValue != v then @watch.signal(x, y, v)

  forEach: (fn) ->
    @rows.forEach (row, x) ->
      row.forEach (v, y) ->
        fn x, y, v



class Jit
  id: -> @nextId++

  # This is some scary / baller shit right here.
  set: (x, y, v) -> @grid.set x, y, v

  constructor: (rawGrid, @opts = {}) ->
    util.printGrid (util.gridExtents rawGrid), rawGrid
    @nextId = 1

    @grid = new Grid()
    for k, v of rawGrid when k not in ['tw', 'th']
      {x,y} = parseXY k
      @grid.set x, y, v

    @engineGrid = new Grid

    @shuttles = new Set
    @shuttles.watch = new Watcher (fn) -> @shuttles.forEach fn
    # This just has the base location of shuttles. Its only useful to find &
    # kill shuttles when they change.
    @shuttleGrid = new Grid


    destroyShuttle = (x, y) =>
      s = @shuttleGrid.get x, y
      return no unless s?.used
      log 'destroying shuttle', s
      s.used = no
      @shuttles.delete s
      @shuttles.watch.signalImm s
      return yes

    makeShuttle = (x, y) =>
      #log 'makeShuttle at', x, y, @grid.get(x, y)
      return unless @grid.get(x, y) in ['shuttle', 'thinshuttle']
      return if (@shuttleGrid.get x, y)?.used

      s =
        id: "s#{@id()}"
        used: yes
        states: []
        size: 0 # For debugging

      @shuttles.add s

      fill {x, y}, (x, y) =>
        destroyShuttle x, y

        if @grid.get(x, y) in ['shuttle', 'thinshuttle']
          @shuttleGrid.set x, y, s
          s.size++
          yes
        else
          no

      throw Error 'empty' unless s.size

      log 'added shuttle', s
      @shuttles.watch.signal s

    @grid.watch.on (x, y, v) =>
      v = @grid.get x, y # needed for txns

      if v in ['positive', 'negative']
        engine =
          pressure: pressureOf v
          regions: []
        @engineGrid.set x, y, engine
      else
        # Destroy any engine that was here
        @engineGrid.set x, y, null

      if v in ['shuttle', 'thinshuttle']
        makeShuttle x, y
      else
        destroyShuttle x, y
        # There might have been a shuttle nearby. Try and make some.
        # This is always needed like this because of txns. :(
        for {dx, dy} in DIRS
          makeShuttle x+dx, y+dy


    #@engines.forEach (x, y, v) -> log x, y, v

    
    log @shuttles
    @shuttleGrid.forEach (x, y, v) -> log x, y, v



# Debugging stuffs
  check: ->
    #SHUTTLES:
    # Invariant: Each cell with a shuttle is part of a shuttle
    @grid.forEach (x, y, v) =>
      if v in ['shuttle', 'thinshuttle']
        s = @shuttleGrid.get x, y
        assert s?.used
        assert @shuttles.has s
    # Invariant: Each cell in the shuttleGrid corresponds to a shuttle cell
    @shuttleGrid.forEach (x, y, s) =>
      return unless s.used
      v = @grid.get x, y
      assert v in ['shuttle', 'thinshuttle']
    # Invariant: Each shuttle in @shuttles is used
    @shuttles.forEach (s) ->
      assert s.used
      assert s.size

    #ENGINES:
    # Invariant: Each engine cell is in the engineGrid
    @grid.forEach (x, y, v) =>
      if v in ['positive', 'negative']
        e = @engineGrid.get x, y
        assert e
        # Invariant: Each engine's pressure is correct
        assert e.pressure is pressureOf v

    # Invariant: Each engine in the engineGrid corresponds to +/-
    @engineGrid.forEach (x, y, e) =>
      assert @grid.get(x, y) in ['positive', 'negative']

  debugPrint: ->
    crapGrid = {}
    @grid.forEach (x, y, v) ->
      crapGrid[[x,y]] = v
    util.printGrid (util.gridExtents crapGrid), crapGrid

    log 'shuttles', @shuttles


  torture: ->
      for [1...1000]
        txn =>
          for [1...10]
            x = mersenne.rand() % 4
            y = mersenne.rand() % 4
            VALUES = ['nothing', null, 'positive', 'negative', 'shuttle', 'thinshuttle']
            v = VALUES[mersenne.rand() % VALUES.length]
            log 'set', x, y, v
            @grid.set x, y, v
        @debugPrint()
        log '-----'

        try
          @check()
        catch e
          @debugPrint()
          throw e

    
parseFile = exports.parseFile = (filename, opts) ->
  fs = require 'fs'
  data = JSON.parse fs.readFileSync(filename, 'utf8').split('\n')[0]
  # Mmmm, resiliancy.
  delete data.tw
  delete data.th
  jit = new Jit data, opts

  jit.torture()
  


if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename



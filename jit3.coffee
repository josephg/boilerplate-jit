{parseXY, fill} = util = require './util'
{Watcher, txn} = require './watch'
{Map2, Set2} = require './collections2'
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


letsShuttleThrough = (v) -> v in ['shuttle', 'thinshuttle', 'nothing']

oppositeDir = (dir) -> (dir+2) % 4

pressureOf = (v) -> if v is 'positive' then 1 else -1

abs = (x) -> if x < 0 then -x else x


randomWeighted = (arr) ->
  totalWeight = 0
  totalWeight += arr[i+1] for _, i in arr by 2
  ->
    r = mersenne.rand() % totalWeight
    for v,i in arr by 2
      r -= arr[i+1]
      break if r < 0

    v


# Iterate through all states in the sidList, calling fn on each.
permuteStates = (shuttleStates, filledStates, fn) ->
  # First, find which shuttles are in play here.
  shuttles = []
  marked = new WeakSet
  filledStates.forEach ({shuttle}) ->
    # The filledStates list will have shuttles multiple times. The set is used
    # to uniq them.
    if !marked.has shuttle
      marked.add shuttle
      shuttles.push shuttle

  # Now lets find out the cross product of those shuttles' states
  totalNumber = 1
  for s in shuttles
    assert s.used
    totalNumber *= shuttleStates.get(s).length

  activeStates = new Map # Map from shuttle -> state for bound states
  for i in [0...totalNumber]
    v = i
    usable = yes
    for s in shuttles
      states = shuttleStates.get(s)
      state = states[v % states.length]
      v = (v / states.length)|0

      if filledStates.has(state)
        log 'unusable' #, activeStates, state
        usable = no
        break
      else
        activeStates.set s, state

    if usable
      log 'state set', activeStates
      fn activeStates

  return



class Jit
  id: -> @nextId++

  # This is some scary / baller shit right here.
  set: (x, y, v) ->
    oldValue = @grid.get x, y
    return if oldValue == v

    if v
      @grid.set x, y, v
    else
      @grid.delete x, y

    @grid.watch.signal x, y

  constructor: (rawGrid, @opts = {}) ->
    @nextId = 1

    @grid = new Map2
    @grid.watch = new Watcher @grid

    for k, v of rawGrid when k not in ['tw', 'th']
      {x,y} = parseXY k
      @grid.set x, y, v

    @engineGrid = new Map2

    @shuttles = new Set
    @shuttles.watch = new Watcher @shuttles
    # This just has the base location of shuttles. Its only useful to find &
    # kill shuttles when they change.
    @shuttleGrid = new Map2

    # Map from shuttle -> [state]
    @shuttleStates = new Map
    @shuttleStates.watch = new Watcher (fn) =>
      @shuttleStates.forEach (states, shuttle) ->
        fn shuttle, state, yes for state in states

    @fillList = new Map2 # Map from (x,y) -> set of states
    @fillList.watch = new Watcher @fillList # Emit (x, y, new list).

    @regionGrid = new Map2 # Map from (x,y) -> each set of states -> region
    #@regions = new Set # All the regions. (Is this needed?)


    @gridToEnginesInit()
    @gridToShuttleInit()
    @shuttleToStatesInit()
    @fillListInit()
    @regionsInit()


    #@engines.forEach (x, y, v) -> log x, y, v

    log '----'
    @debugPrint()
    
    #log @shuttles
    #@shuttleGrid.forEach (x, y, v) -> log x, y, v

  # ------- Grid -> Engines --------
  gridToEnginesInit: ->
    @grid.watch.on (x, y) =>
      v = @grid.get x, y

      if v in ['positive', 'negative']
        engine =
          pressure: pressureOf v
          regions: []
        @engineGrid.set x, y, engine
      else
        # Destroy any engine that was here
        @engineGrid.delete x, y


  # ------- Grid -> Shuttles --------
  gridToShuttleInit: ->
    @grid.watch.on (x, y) =>
      v = @grid.get x, y

      if v in ['shuttle', 'thinshuttle']
        @makeShuttle x, y
      else
        @destroyShuttle x, y
        # There might have been a shuttle nearby. Try and make some.
        # This is always needed like this because of txns. :(
        for {dx, dy} in DIRS
          @makeShuttle x+dx, y+dy

  destroyShuttle: (x, y) ->
    s = @shuttleGrid.get x, y
    return no unless s?.used
    log 'destroying shuttle', s
    s.used = no
    @shuttles.delete s
    @shuttles.watch.signalImm s
    return yes

  makeShuttle: (x, y) ->
    #log 'makeShuttle at', x, y, @grid.get(x, y)
    v = @grid.get x, y
    return unless v in ['shuttle', 'thinshuttle']

    s = @shuttleGrid.get x, y
    return if s?.used and s?.points.get(x, y) is v

    s =
      id: "s#{@id()}"
      used: yes
      size: 0 # For debugging
      points: new Map2

    @shuttles.add s

    fill {x, y}, (x, y) =>
      @destroyShuttle x, y
      
      v = @grid.get(x, y)
      if v in ['shuttle', 'thinshuttle']
        @shuttleGrid.set x, y, s
        s.size++
        s.points.set x, y, v
        yes
      else
        no

    throw Error 'empty' unless s.size

    log 'added shuttle', s
    @shuttles.watch.signal s


  # ------- Shuttles -> State --------
  shuttleToStatesInit: ->
    @shuttles.watch.on (shuttle) =>
      if shuttle.used
        # The shuttle has just been made. Make a fresh shuttle state list for
        # it.
        @createStateAt shuttle, 0, 0
      else
        states = @shuttleStates.get shuttle
        @shuttleStates.delete shuttle
        @shuttleStates.watch.signalImm shuttle, state, no for state in states

    #@grid.watch.on (x, y) ->
    #  v = @grid.get x, y

    # Here we need to find any states that contain (x,y) and regenerate them
    # 
    # Also we need to regen the flags on any state bordering on this grid cell.

  canShuttleFitAt: (shuttle, dx, dy) ->
    shuttle.points.forEach (x, y) =>
      # The grid cell is un-enterable.
      if !letsShuttleThrough @grid.get(x+dx, y+dy)
        return no
    return yes

  calcFlags: (shuttle, dx, dy) ->
    flags = 0
    for {dx:ddx, dy:ddy}, i in DIRS
      flags |= (1<<i) if @canShuttleFitAt shuttle, dx + ddx, dy + ddy
    flags

  createStateAt: (shuttle, dx, dy) ->
    # First check that a state can exist here. Not entirely sure this check is
    # needed given that:
    # - At 0,0 the shuttle will always fit
    # - Anywhere else, we have flags on each state created
    return null unless @canShuttleFitAt shuttle, dx, dy

    state =
      dx: dx
      dy: dy
      flags: @calcFlags shuttle, dx, dy
      successor: [-1, -1, -1, -1]
      shuttle: shuttle

    states = @shuttleStates.get shuttle
    if states
      states.push state
    else
      @shuttleStates.set shuttle, [state]

    @shuttleStates.watch.signal shuttle, state, yes
    log 'created new state', state

    state

    ###
    for {x, y} in shuttle.points
      v = @grid.get x, y
      assert v in ['shuttle', 'thinshuttle']

      x += dx; y += dy
    ###

  # ------- State -> Fill List --------

  fillListInit: ->
    @shuttleStates.watch.on (shuttle, state, isCreated) =>
      log 'fli', shuttle, state, isCreated

      if isCreated
        return unless shuttle.used

        shuttle.points.forEach (x, y, v) =>
          assert v in ['shuttle', 'thinshuttle']
          return unless v is 'shuttle'

          x += state.dx; y += state.dy

          log 'adding to fill list at', x, y
          
          # Add state to fillList at x, y
          set = @fillList.get x, y
          if set
            set.add state
          else
            set = new Set
            set.add state
            @fillList.set x, y, set

          @fillList.watch.signal x, y, set
      else
        # Kapowey!
        log 'Kapowey'
        shuttle.points.forEach (x, y, v) =>
          x += state.dx; y += state.dy
          set = @fillList.get x, y
          return unless set
          set.delete state
          log 'removing state from', x, y
          @fillList.watch.signal x, y, set

  # ------- fill list -> regions --------

  regionsInit: ->
    @fillList.watch.on (x, y, states) =>
      log 'fill list', x, y, states

      permuteStates @shuttleStates, states, (activeStates) ->
        log 'active states', activeStates




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
    @shuttles.forEach (s) =>
      assert s.used
      assert s.size
    # Invariant: Points list matches
      
      s.points.forEach (x, y, v) =>
        assert.equal v, @grid.get x, y
        assert v in ['shuttle', 'thinshuttle']
        for {dx, dy} in DIRS when !s.points.has x+dx, y+dy
          v2 = @grid.get x+dx, y+dy
          #log 'v2 at', x+dx, y+dy, v2
          assert v2 not in ['shuttle', 'thinshuttle']


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

    #STATES:
    # Invariant: There is a shuttle state list for all shuttles
    @shuttles.forEach (s) =>
      states = @shuttleStates.get s
      assert states
      assert states.length >= 1
      stateAt = new Set2
      for state in states
        assert !stateAt.has state.dx, state.dy
        stateAt.add state.dx, state.dy
    # Invariant: There is no shuttle state list for unused shuttles
    @shuttleStates.forEach (states, s) =>
      assert s.used
      states.forEach (state) ->
        assert.equal state.shuttle, s
    
    #FILL LIST:
    @fillList.forEach (x, y, set) =>
      set.forEach (state) =>
    # Invariant: Each state in the fill list has a live shuttle
        assert state.shuttle.used

    # Invariant: Each fill in the fill list corresponds to a shuttle grid cell
        log 'xx', x, y, state.dx, state.dy, state
        x = x - state.dx; y = y - state.dy
        v = @grid.get x, y
        assert.equal v, 'shuttle'

  debugPrint: ->
    crapGrid = {}
    @grid.forEach (x, y, v) ->
      crapGrid[[x,y]] = v
    util.printGrid (util.gridExtents crapGrid), crapGrid

    log 'shuttles', @shuttles

    #@fillList.forEach (x, y, s) ->
    #  log 'fl', x, y, s


  torture: ->
    randomValue = randomWeighted [
      null, 2
      'nothing', 10
      'positive', 1
      'negative', 1
      'shuttle', 1
      'thinshuttle', 1
    ]

    for [1...1000]
      txn =>
        for [1...10]
          x = mersenne.rand() % 4
          y = mersenne.rand() % 4
          v = randomValue()
          log 'set', x, y, v
          @set x, y, v
      @debugPrint()
      log '-----'

      try
        @check()
      catch e
        log '****** CRASH ******'
        @debugPrint()
        throw e

    
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

  jit.torture() if torture
  


if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename



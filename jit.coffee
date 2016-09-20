# This jit is designed as an ecosystem of signaling parts.
Watcher = require './watch'
{Map2, Map3, Set2, Set3, SetOfPairs} = require './collections2'
{parseXY, fill, DIRS} = util = require './util'
log = require './log'
assert = require 'assert'

UP=0; RIGHT=1; DOWN=2; LEFT=3

makeId = do ->
  nextId = 1
  -> nextId++

# Its super arbitrary whether shuttles can go through insulated wire. I'm
# letting them go through ribbonbridge because its just too convenient.
letsShuttleThrough = (v) -> v in ['nothing', 'bridge', 'ribbon', 'ribbonbridge']

log.quiet = yes

abs = (x) -> if x >= 0 then x else -x

SHUTTLE     = 0x40
THINSHUTTLE = 0x80

normalizeShuttleV = (v) ->
  # Shuttle values use the highest 2 bits for the value (0x80 and 0x40 for
  # shuttle / thinshuttle).
  #
  # The low 4 bits describe the connectivity with adjacent cells. The
  # connectivity values must always be consistent with adjacent cells. The
  # connectivity on a newly set shuttle cell takes precendence.
  return v if typeof v is 'number'

  return if v is 'shuttle'
    SHUTTLE | 0b1111
  else if v is 'thinshuttle'
    THINSHUTTLE | 0b1111
  else
    assert.equal v, null
    0

shuttleConnects = (sv, dir) -> !!(sv & (1<<dir))

# Used in sorting functions to make the simulation stable. Sorts a list of
# shuttles by their anchor points (top to bottom, left to right)
compareByPosition = (a, b) ->
  # Could cache the anchor for each shuttle state, but I don't think it
  # would buy us anything.
  ay = a.anchor.y + a.currentState.dy
  _by = b.anchor.y + b.currentState.dy # 'by' is a coffeescript keyword
  return ay - _by if ay != _by

  ax = a.anchor.x + a.currentState.dx
  bx = b.anchor.x + b.currentState.dx
  return ax - bx


BaseGrid = ->
  # This stores the base layer of cells for the world - which is to say,
  # whatever is underneath shuttles.
  #
  # Shuttles aren't stored here (it doesn't make sense to store them in a grid
  # because they're moving dawg). For that, we have ShuttleBuffer (below).
  grid = new Map2

  forEach = (fn) ->
    grid.forEach (x, y, v) ->
      fn x, y, null, v

  # You usually want to use afterWatch to invalidate things. beforeWatch is
  # important for collapsing shuttles.
  beforeWatch = new Watcher forEach
  afterWatch = new Watcher forEach

  beforeWatch: beforeWatch
  afterWatch: afterWatch
  get: grid.get.bind grid
  set: (x, y, v) ->
    assert !v? or typeof v is 'string'
    v = undefined if v is null or v is 'solid'
    assert v not in ['shuttle', 'thinshuttle']
    oldv = grid.get x, y
    beforeWatch.signal x, y, oldv, v
    # oldv might have changed as a result of signaling.
    oldv = grid.get x, y

    if v != oldv
      if v
        grid.set x, y, v
      else
        grid.delete x, y
      afterWatch.signal x, y, oldv, v
      return yes
    else
      return no
  forEach: grid.forEach.bind grid

  checkEmpty: -> assert.strictEqual 0, grid.size

pump = (grid) -> (x, y) ->
  v = grid.get x, y
  grid.delete x, y if v
  return v

BaseBuffer = (grid, values) ->
  # EngineBuffer wraps a Map2 grid to use as a buffer between cells in the grid
  # and engines.
  #
  # When cells are turned into engines, they get deleted from the grid. Then
  # they get added back when the shuttle / engine is deleted.
  buffer = new Map2
  watch = new Watcher

  grid.afterWatch.forward (x, y, oldv, v) ->
    if oldv in values
      # This working depends on EngineGrid, below.
      assert.equal buffer.get(x, y), oldv
      buffer.delete x, y
      watch.signal x, y

    if v in values
      buffer.set x, y, v
      watch.signal x, y, v

  watch: watch
  data: buffer
  pump: pump buffer

ShuttleBuffer = ->
  # The shuttle buffer is like EngineBuffer, but when shuttles need to go back
  # in here is harder to figure out - we need semantic information about where
  # the shuttle is to both know when to delete it, and know where to put it. So
  # that happens somewhere else.
  
  # A grid of unrealised shuttle cells. Values are 
  buffer = new Map2
  watch = new Watcher # This is a beforeWatch

  set: (x, y, v) ->
    #assert !isNaN x
    v = normalizeShuttleV v

    # Bake down adjacent shuttles no matter what.
    watch.signal x, y, v

    if v
      # Important we do this after signalling - since there might be a shuttle
      # here to delete.
      return if buffer.get(x, y) is v
      for {dx, dy},d in DIRS
        connects = shuttleConnects v, d
        x2 = x+dx; y2 = y+dy
        v2 = buffer.get x2, y2
        if v2
          # Weld adjacent shuttle cells to the new cell.
          connects2 = shuttleConnects v2, (d2 = util.oppositeDir d)
          if connects != connects2
            if connects
              v2 |= 1<<d2
            else
              v2 &= ~(1<<d2)
            buffer.set x2, y2, v2
        else
          # Nothing here. Don't connect the new cell.
          v &= ~(1<<d)

      buffer.set x, y, v
    else
      # Disconnect adjacent shuttle cells from this one
      oldV = buffer.get x, y
      for {dx, dy},d in DIRS when shuttleConnects oldV, d
        x2 = x+dx; y2 = y+dy
        v2 = buffer.get x2, y2
        d2 = util.oppositeDir d
        assert shuttleConnects v2, d2
        v2 &= ~(1<<d2)
        buffer.set x2, y2, v2

      buffer.delete x, y

  watch: watch
  data: buffer
  pump: pump buffer

# The code for finding & flood filling shuttles & engines is basically
# identical. I'll reuse it, and switch according to which type of thing I'm
# looking for.
BlobFiller = (type, buffer) ->
  throw Error 'Invalid type' unless type in ['shuttle', 'engine']

  blobs = new Set
  addWatch = new Watcher (fn) -> blobs.forEach fn
  deleteWatch = new Watcher

  # Shuttles and engines get destroyed using ShuttleGrid and EngineGrid
  # respectively - which call .delete()
  deleteBlob = (b, pos) ->
    return no unless blobs.delete b

    assert !pos || pos.dx?

    #console.trace "deleteBlob #{dx},#{dy}"

    log "Destroyed #{type} #{b.id} at", b.points
    assert b.used
    b.used = no
    # Put the points we ate back in the buffer.
    if pos
      {dx, dy} = pos
    else
      dx = dy = 0
    b.points.forEach (x2, y2, v) ->
      # Bypassing ShuttleBuffer's watch function. Is this correct..?
      buffer.data.set x2+dx, y2+dy, v

    deleteWatch.signal b
    return yes

  Blob = (x, y, v0) ->
    #v0 = buffer.peek x, y
    #assert buffer.data.get(x, y) in match

    @id = makeId()
    @used = yes
    @size = 0 # For debugging
    @points = new Map2
    # Edges around the entire shuttle
    @edges = new Set3
    # Edges around v=shuttle tiles
    if type is 'shuttle'
      @pushEdges = new Set3
      @numValidStates = 0
      @currentState = null
      @eachCurrentPoint = (fn) ->
        dx = if @currentState then @currentState.dx else 0
        dy = if @currentState then @currentState.dy else 0
        @points.forEach (x, y, v) -> fn x+dx, y+dy, v
      @blockedX = @blockedY = null

      # This stuff could be held in a weakmap by Step, but its hot data
      # and I _think_ it'll be faster to embed it directly in the shuttle than
      # look it up all the time during step(). Its kinda gross that this is
      # here though.
      @stepTag = 0
      @imX = @imY = 0 # Used to sort
      @imXRem = @imYRem = 0 # What gets actually consumed when the shuttle moves
      # Set of zones which, when deleted, will wake the shuttle
      @zoneDeps = new Set

      # These sets are going to be *tiny* - everything in them needs to be
      # physically touching the shuttle. An ideal representation would be a
      # list that turns into a hashset if it grows beyond a certain size. Or
      # just a list, and eat the O(n) time for some operations if a shuttle
      # touches lots of other shuttles.
      @shuttleDeps = new Set
      

    @anchor = {x, y} # leftmost cell in the top row

    blobs.add this

    # The third parameter is really just cached via the fill mechanism... but
    # it means I don't need both fill2 and fill3 in util.
    util.fill3 x, y, v0, (x, y, v, hmm) =>
      buffer.pump x, y

      @size++
      @points.set x, y, v

      if y < @anchor.y || (y == @anchor.y && x < @anchor.x)
        @anchor.x = x
        @anchor.y = y

      for {dx, dy},d in DIRS
        x2 = x+dx; y2 = y+dy
        v2 = @points.get(x2, y2) || buffer.data.get(x2, y2)

          #continue if type is 'shuttle' and 
        if v2 and ((type is 'shuttle' and shuttleConnects v, d) or
            (type is 'engine' and v2 == v))

          if type is 'shuttle' then assert shuttleConnects(v2, util.oppositeDir d)

          hmm x2, y2, v2
        else
          @edges.add x, y, d

        if type is 'shuttle' and v & SHUTTLE and (!v2 or !(v2 & SHUTTLE) or !shuttleConnects(v, d))
          @pushEdges.add x, y, d

    assert @size
    assert @pushEdges.size is 0 or @pushEdges.size >= 4 if type is 'shuttle'
    if type is 'engine'
      @type = v0
      @pressure = (if v0 is 'positive' then 1 else -1) * @size

    log @id, "Added #{type}", this
    addWatch.signal this
    return

  addWatch: addWatch
  deleteWatch: deleteWatch

  flush: ->
    #console.log('flush', (new Error).stack)
    buffer.data.forEach (x, y, v) ->
      #assert !isNaN x
      new Blob x, y, v
    assert.equal buffer.data.size, 0

  flushAt: (x, y) ->
    if v = buffer.data.get x, y
      return new Blob x, y, v

  forEach: (fn) ->
    @flush()
    blobs.forEach fn

  delete: deleteBlob

  check: (invasive) ->
    @forEach(->) if invasive
    blobs.forEach (b) =>
      assert b.used
      b.points.forEach (x, y, v) =>
        if type is 'engine' then assert !buffer.data.has x, y

        # - Shuttles always have all adjacent shuttle / thinshuttle points
        for {dx, dy},d in DIRS
          if b.points.has x+dx,y+dy
            if type is 'shuttle'
              v2 = b.points.get x+dx, y+dy
              c1 = shuttleConnects v, d
              c2 = shuttleConnects v2, util.oppositeDir d
              assert.equal c1, c2, "Mismatched adjacency in a shuttle: #{c1} #{c2}"
          else
            # Make sure we've slurped up all the dangling grid cells
            if type is 'engine'
              v2 = buffer.data.get x+dx, y+dy
              assert v2 != v
            else if type is 'shuttle'
              assert !shuttleConnects v, d
              assert b.pushEdges.has x, y, d if v & SHUTTLE

# Super simple module to get the engine at an arbitrary grid cell (x, y).
EngineGrid = (grid, engines) ->
  engineGrid = new Map2 # x,y -> engine

  grid.beforeWatch.forward (x, y, oldv, v) ->
    if oldv in ['positive', 'negative'] and (e = engineGrid.get x, y)
      #log 'deleting engine at', x, y, e
      engines.delete e

    # Engines really don't care about anything else.
    if v in ['positive', 'negative']
      # Reap adjacent cells. This might be special for shuttles?
      for {dx, dy} in DIRS when (e = engineGrid.get x+dx, y+dy)
        engines.delete e

  engines.addWatch.forward (engine) ->
    engine.points.forEach (x, y, v) ->
      engineGrid.set x, y, engine

  engines.deleteWatch.on (engine) ->
    engine.points.forEach (x, y) -> engineGrid.delete x, y

  get: (x, y) ->
    e = engineGrid.get x, y
    if !e
      return engines.flushAt x, y
    else
      return e

  check: (invasive) ->
    engineGrid.forEach (x, y, e) ->
      assert e.used
      # - No two engines of the same type should be adjacent
      for {dx, dy} in DIRS when (e2 = engineGrid.get x+dx, y+dy)
        assert e2 == e or e.type != e2.type

      # - Engines are represented in the grid
      e.points.forEach (x, y, v) ->
        assert.equal engineGrid.get(x, y), e



ShuttleStates = (baseGrid, shuttles) ->
  # The set of shuttle states. A shuttle's state list starts off with just one
  # entry (the shuttle's starting state). States are added when the shuttle is
  # created or the shuttle moves.
  #
  # This module is kind of incomplete - when the grid changes, generated states
  # aren't removed from the state grid. They're only removed when the shuttle
  # itself is deleted. In practice, I do that work in CollapseDetector below -
  # but it still irks me. These things should be self contained.
  shuttleStates = new Map # shuttle -> Map2(dx, dy) -> states

  addWatch = new Watcher (fn) ->
    shuttleStates.forEach (shuttle, states) ->
      states.forEach (x, y, state) ->
        fn state
  deleteWatch = new Watcher

  shuttles.deleteWatch.on (shuttle) ->
    # Shuttle delete messages
    states = shuttleStates.get shuttle
    if states
      shuttleStates.delete shuttle
      states.forEach (x, y, state) ->
        deleteWatch.signal state

  #shuttles.addWatch.forward (shuttle) ->
  #  # This isn't necessary because CurrentStates will call getInitialState() anyway.
  #  createStateAt shuttle, 0, 0

  canShuttleFitAt = (shuttle, dx, dy) ->
    fits = yes
    shuttle.points.forEach (x, y) =>
      # The grid cell is un-enterable.
      fits = no if !letsShuttleThrough baseGrid.get(x+dx, y+dy)
    return fits

  createStateAt = (shuttle, dx, dy) ->
    states = shuttleStates.get shuttle

    valid = canShuttleFitAt shuttle, dx, dy

    state =
      dx: dx
      dy: dy
      valid: valid
      shuttle: shuttle
      id: if valid then shuttle.numValidStates else -1
      stepTag: 0 # For the state shadow. Overlapped concerns for optimization.
    shuttle.numValidStates++ if valid

    if states
      states.set dx, dy, state
    else
      shuttleStates.set shuttle, new Map2 [[dx, dy, state]]

    #log 'created new state', state
    log 'made shuttle state for shuttle', state.id, state.shuttle.id, state.dx, state.dy if valid
    addWatch.signal state
    return state

  flushStatesAt: (x, y) ->
    # Its kind of gross that I need this. Anyone listening on our watcher will
    # see states as they get created, but a shuttle might not have been created
    # yet (because of how the shuttle group works).
    #
    # NEVER CALL ME on a burning pass - or you might get wacky N^2 behaviour.
    shuttles.flushAt x, y
    #s = shuttles.get x, y
    #@get s if s

  addWatch: addWatch
  deleteWatch: deleteWatch
  get: (s) -> shuttleStates.get(s)

  getInitialState: (s) -> @get(s)?.get(0,0) or createStateAt s, 0, 0

  collapse: (shuttle) ->
    log 'collapsing', shuttle

    saved = shuttle.currentState
    # Delete all states except for the current state
    return unless (states = shuttleStates.get shuttle)
    states.forEach (dx, dy, state) ->
      return if state is saved # Hallelujah!
      states.delete dx, dy
      deleteWatch.signal state

  getStateNear: (state, dir) -> # Dir is a number.
    assert state.shuttle.used
    return null unless state.valid

    {dx, dy} = DIRS[dir]
    dx += state.dx; dy += state.dy
    successor = shuttleStates.get(state.shuttle).get dx, dy

    if !successor?
      successor = createStateAt state.shuttle, dx, dy

    return successor if successor.valid

  delete: (state) ->
    log 'deleting state', state
    shuttle = state.shuttle
    assert shuttle.used
    shuttleStates.get(shuttle).delete state.dx, state.dy
    deleteWatch.signal state

ShuttleGrid = (shuttleStates) ->
  # This stores a couple data structures describing how the shuttles interact
  # with the grid.
  #
  # This is updated eagerly when shuttle states are created.

  # Set of states which fill a given grid cell (which is to say, block pressure)
  #
  # This is used for figuring out cell groups.
  fillGrid = new Map2 -> new Set # Map from (x,y) -> set of states
  fillWatch = new Watcher

  # Set of states which fill a grid cell (including invalid states).
  # Its different from fillGrid in two ways:
  #  1. It includes any shuttle which occupies a grid cell using thinshuttle
  #  2. It includes invalid states
  #
  # Its used to find & kill shuttles when the grid changes and also make sure
  # shuttles don't intersect.
  stateGrid = new Map2 -> new Set
  stateWatch = new Watcher

  shuttleStates.addWatch.forward (state) ->
    #log 'shuttleStates signal', state, created
    # Populate the fill list
    state.shuttle.points.forEach (x, y, v) ->
      x += state.dx; y += state.dy

      stateGrid.getDef(x, y).add state
      stateWatch.signal x, y

      if v & SHUTTLE and state.valid
        #log "adding #{state.id} to #{x}, #{y}"
        fillGrid.getDef(x, y).add state
        fillWatch.signal x, y

  shuttleStates.deleteWatch.on (state) ->
    log 'shuttle grid removing', state.shuttle.id, state.dx, state.dy
    state.shuttle.points.forEach (x, y, v) ->
      x += state.dx; y += state.dy

      stateGrid.get(x, y).delete state
      stateWatch.signal x, y

      if v & SHUTTLE and state.valid
        fillGrid.get(x, y).delete state
        fillWatch.signal x, y

  fillGrid: fillGrid
  fillWatch: fillWatch
  stateGrid: stateGrid
  stateWatch: stateWatch

  getStates: (x, y) -> stateGrid.get(x, y)

  getShuttle: (x, y) ->
    # Get the shuttle currently at (x, y), or null.
    shuttle = null
    stateGrid.get(x, y)?.forEach (state) ->
      if state.shuttle.currentState is state
        shuttle = state.shuttle
    return shuttle

  getValue: (x, y) ->
    # Get the grid cell value at x, y. This is a lot of work - if you want to
    # draw a lot of shuttles or something, don't use this.
    return unless (shuttle = @getShuttle x, y)
    {dx, dy} = shuttle.currentState
    return shuttle.points.get x-dx, y-dy

  check: ->

FillKeys = (baseGrid, shuttleStates, shuttleGrid) ->
  # The fill key is used for cell groups. Every adjacent grid cell with the
  # same fill key will always have the same pressure value regardless of
  # shuttle state. For now, the key is just a (stable) list of the states in
  # the fill grid.
  fillKey = new Map2 # Map from (x,y) -> string key

  # Set of the actual states which fill for a particular key
  fillStates = new Map # Map from key -> set of states
  fillStates.default = -> new Set
  fillStates.set '', new Set # Used *everywhere*.

  # For cleaning up fillStates when a state is removed.
  # Map from state -> set of keys
  keysReferencingState = new WeakMap
  keysReferencingState.default = -> new Set

  watch = new Watcher

  shuttleGrid.fillWatch.on (x, y) ->
    fillKey.delete x, y

  baseGrid.afterWatch.on (x, y, oldv, v) ->
    fillKey.delete x, y if letsShuttleThrough oldv

  shuttleStates.deleteWatch.on (state) ->
    # We don't need to update fillKey because the fillWatch above will take
    # care of it.
    keysReferencingState.get(state)?.forEach (key) ->
      fillStates.delete key
      watch.signal key

  calcKeyAt = (x, y) ->
    stateList = []
    shuttleGrid.fillGrid.get(x, y)?.forEach (state) -> stateList.push state
    stateList.sort (s1, s2) ->
      if s1.shuttle != s2.shuttle
        s1.shuttle.id - s2.shuttle.id
      else
        s1.id - s2.id

    key = stateList
      .map (state) -> "#{state.shuttle.id}.#{state.id}"
      .join ' '

    if !fillStates.has key
      # It'd be nice to simply reference the same set as in fillGrid, but we
      # can't - the set in fillGrid changes based on the states of that grid
      # cell. This set is immutable.
      set = fillStates.getDef key
      set.add state for state in stateList
    for state in stateList
      keysReferencingState.getDef(state).add key

    return key

  watch: watch
  getFilledStates: (key) -> fillStates.get key
  getFillKey: (x, y) ->
    return '' if !letsShuttleThrough baseGrid.get(x,y)
    shuttleStates.flushStatesAt x, y
    key = fillKey.get x, y
    if !key
      key = calcKeyAt x, y
      fillKey.set x, y, key
    return key

  checkEmpty: ->
    assert.equal 0, fillKey.size
    assert.equal 1, fillStates.size


Groups = (baseGrid, engines, engineGrid, shuttleGrid, fillKeys) ->
  # The first step to calculating regions is finding similar cells. Similar
  # cells are neighboring cells which will always (come hell or high water)
  # contain the same regions. (And same pressure value)
  #
  # Eg:
  # **********
  # *123S0000*
  # *123S*****
  # ******
  #
  # This is very similar to shuttles, but instead of flood filling on connected
  # shuttle cells, we'll connect cells which have the same FillKey.

  # (x, y, cell) -> cell value. This works the same as the buffers for engines
  # and shuttles.
  pendingCells = new Set3

  groups = new Set

  # (x, y, cell) -> group
  groupGrid = new Map3

  # Needed for gc - this is a set of groups which depend on the fillgrid at x,y.
  edgeGrid = new Map2 -> new Set

  addWatch = new Watcher groups
  deleteWatch = new Watcher

  # For garbage collection.
  groupsWithEngine = new WeakMap
  groupsWithEngine.default = -> new Set

  deleteGroupsAt = (x, y) ->
    cmax = util.cellMax baseGrid.get x, y
    for c in [0...cmax]
      if (group = groupGrid.get x, y, c)
        deleteGroup group

  deleteGroup = (group) ->
    log group._id, ': deleting group', group._id
    assert group.used

    # Invalidate g!
    group.used = no # More for debugging than anything.
    groups.delete group

    group.engines.forEach (e) ->
      groupsWithEngine.get(e).delete group

    # Recycle the points
    group.points.forEach (px, py, pc, pv) ->
      #log 'pcb', px, py, pc, pv
      pendingCells.add px, py, pc
      groupGrid.delete px, py, pc

    group.edges.forEach (x, y, c) ->
      edgeGrid.get(x, y).delete group
    deleteWatch.signal group

  baseGrid.afterWatch.forward (x, y, oldv, v) ->
    cmax = util.cellMax oldv
    for c in [0...cmax]
      #log '---', x, y, c, oldv
      if (group = groupGrid.get x, y, c)
        deleteGroup group
      pendingCells.delete x, y, c

    deleteGroupsAt x+dx, y+dy for {dx, dy} in DIRS

    pendingCells.add x, y, c for c in [0...util.cellMax v]

  engines.deleteWatch.on (e) ->
    # Because part of an engine which isn't adjacent to the group can be
    # deleted / changed, we won't always know to delete the group from looking
    # at the grid.
    set = groupsWithEngine.get e
    if set
      #log 'deleted engine', e
      set.forEach (g) -> deleteGroup g
      groupsWithEngine.delete e

  shuttleGrid.fillWatch.on (x, y) ->
    deleteGroupsAt x, y
    # Groups will stop spreading based on adjacent fill keys.
    edgeGrid.get(x, y)?.forEach (g) -> deleteGroup g

  makeGroupAt = (x, y, c) ->
    v0 = baseGrid.get x, y

    #log 'makeGroupAt', x, y, c, v0
    assert v0?
    assert c < util.cellMax v0
    assert pendingCells.has x, y, c

    key = fillKeys.getFillKey x, y
    filledStates = fillKeys.getFilledStates key
    shuttles = util.uniqueShuttlesInStates filledStates

    group =
      _id: makeId()
      used: yes
      size: 0 # For debugging
      fillKey: key # We could just request this out of fillKeys, but its handy.
      points: new Map3
      edges: new Set3 # x, y, c
      shuttles: shuttles
      shuttleKey: shuttles.map((s) -> "#{s.id}").join ' '
      # Does the group *only* have engine cells? They clutter up my logs and make me mad!
      useless: true
      engines: new Set # Engines this group contains

    log group._id, ': makeGroupAt', x, y, c, "'#{key}'"

    util.fill3 x, y, c, (x, y, c, hmm) ->
      v = baseGrid.get x, y
      #log 'fillCells', x, y, c, v
      return unless v # Optimization.

      group.useless = no if v and v not in ['positive', 'negative']
      if fillKeys.getFillKey(x, y) != key
        #log "wrong fill key -> '#{fillKeys.getFillKey(x, y)}'", x, y, c
        group.edges.add x, y, c
        edgeGrid.getDef(x, y).add group
        return
      log 'fillCells', x, y, c, v

      group.points.set x, y, c, v
      group.size++

      assert !groupGrid.has x, y, c
      groupGrid.set x, y, c, group

      assert pendingCells.has x, y, c
      pendingCells.delete x, y, c

      if v in ['positive', 'negative']
        e = engineGrid.get x, y
        group.engines.add e
        groupsWithEngine.getDef(e).add group

      # Hmm for each adjacent cell.
      for [x2, y2, c2] in util.connectedCells baseGrid, x, y, c
        hmm x2, y2, c2

      return

    groups.add group
    log group._id, ': made group', group.points unless group.useless
    assert group.size
    assert group.used
    addWatch.signal group
    return group

  addWatch: addWatch
  deleteWatch: deleteWatch

  get: (x, y, c) ->
    g = groupGrid.get x, y, c

    if !g
      v = baseGrid.get x, y
      assert 0 <= c < util.cellMax(v)

      # I don't think there's really any value in which we don't make a cell
      # group. I mean, its dumb making a group underneath an
      # immovable shuttle - but who's counting?
      if v?
        g = makeGroupAt x, y, c

    return g if !g.useless

  getDir: (x, y, dir) ->
    v = baseGrid.get x, y
    return unless v
    return if v in ['ribbon', 'ribbonbridge']

    c = switch v
      when 'positive', 'negative'
        dir
      when 'bridge'
        dir % 2
      else
        0

    @get x, y, c

  flush: ->
    pendingCells.forEach (x, y, c) ->
      makeGroupAt x, y, c

  forEach: (fn) ->
    # Fetching all groups will force them all to get generated.
    # Probably more useful for debugging than anything.
    @flush()
    groups.forEach (g) ->
      fn g if !g.useless

  check: check = ->
    groups.forEach (g) ->
      g.points.forEach (x, y, c) ->
        assert.equal groupGrid.get(x, y, c), g

  checkEmpty: ->
    assert.equal 0, groups.size
    assert.equal 0, groupGrid.size
    assert.equal 0, pendingCells.size

StateForce = (grid, shuttleStates, shuttleGrid, groups) ->
  # For each shuttle state, we need to know which groups apply a force.
  stateForce = new Map # state -> force.
  stateForce.default = (state) -> makeForce state

  stateForGroup = new Map # group -> set of states
  stateForGroup.default = -> new Set

  watch = new Watcher

  # This could be eager or lazy. If we're eager there's a potential race
  # condition: a new state is created, the fillgrid invalidates some groups
  # then we look at those groups to determine who applies force. If we look at
  # the groups before they're invalidated, it could give us n^2 performance.
  #
  # So we'll just generate forces lazily, when the shuttle is actually needed.
  shuttleStates.deleteWatch.on (state0) ->
    deleteForce state0
    # Cross of death. If an adjacent invalid state was removed, we will have
    # discounted forces pushing in that direction.
    if (states = shuttleStates.get state0.shuttle)
      for {dx, dy} in DIRS
        state = states.get state0.dx+dx, state0.dy+dy
        deleteForce state if state

  grid.afterWatch.on (x, y, oldv, v) ->
    # Forces depend on the groups nearby. When the nearby groups are GCed,
    # thats fine - we'll destroy the force. But sometimes there is no group
    # next to the shuttle. When a new cell is added there, we need to
    # recalculate the force to add the group.
    #
    # This is only important for filled cells at the moment - but that may
    # change.
    if util.cellMax(oldv) is 0 then for {dx, dy} in DIRS
      states = shuttleGrid.getStates x+dx, y+dy
      states?.forEach deleteForce

  groups.deleteWatch.on (group) ->
    #log.quiet = no if group._id is 60114
    log 'got group deleted', group._id
    if (set = stateForGroup.get group)
      set.forEach (state) -> deleteForce state

      # These should be cleaned up in deleteForce when they're empty.
      assert !stateForGroup.has group

  deleteForce = (state) ->
    if (force = stateForce.get state)
      stateForce.delete state

      log 'deleteForce', state.shuttle.id, state.dx, state.dy

      force.used = no

      delGroups = (pressure, group) ->
        if (set = stateForGroup.get group)
          set.delete state
          stateForGroup.delete group if set.size is 0

      force.x?.forEach delGroups
      force.y?.forEach delGroups

      watch.signal state, force

  makeForce = (state) ->
    log 'makeForce', state.shuttle.id, state.dx, state.dy
    assert state.shuttle.used
    assert state.valid

    canMoveX = shuttleStates.getStateNear(state, LEFT) or
      shuttleStates.getStateNear(state, RIGHT)
    canMoveY = shuttleStates.getStateNear(state, UP) or
      shuttleStates.getStateNear(state, DOWN)

    # A force in a given direction is either null or a map from group -> how
    # much pressure that group exerts.
    force =
      x: if canMoveX then new Map
      y: if canMoveY then new Map
      used: yes

    force.x?.default = -> 0
    force.y?.default = -> 0

    if canMoveX || canMoveY then state.shuttle.pushEdges.forEach (x, y, dir) ->
      x += state.dx; y += state.dy
      if dir in [LEFT, RIGHT]
        return if !canMoveX
        map = force.x
        f = if dir is LEFT then -1 else 1
      else
        return if !canMoveY
        map = force.y
        f = if dir is UP then -1 else 1

      {dx, dy} = DIRS[dir]

      log 'edge', x, y
      log 'looking in', x+dx, y+dy, util.oppositeDir dir

      group = groups.getDir x+dx, y+dy, util.oppositeDir dir
      return unless group

      map.set group, map.getDef(group) + f

    for map in [force.x, force.y] when map
      map.forEach (pressure, group) ->
        if pressure is 0
          map.delete group
        else
          stateForGroup.getDef(group).add state

    #log '-> makeForce', force
    force

  watch: watch
  get: (state) ->
    f = stateForce.getDef state
    assert f.used
    f


GroupConnections = (groups) ->
  # So, here we track & report on connections between groups. Groups are either
  # complete or incomplete. Incomplete groups need to be regenerated if
  # requested, but they are useful for garbage collection.
 
  # A group is always regenerated if a new group is drawn next to it.

  connections = new SetOfPairs # Map from group -> set of groups it touches
  complete = new WeakSet # Set of groups which we have all the connections of

  groups.deleteWatch.on (group) ->
    #log 'deleting'

    # Any groups we've cached which connect to the deleted group will
    # need to be regenerated.
    if (gc = connections.getAll group)
      gc.forEach (g2) ->
        complete.delete g2

    connections.deleteAll group

  findConnections = (group) ->
    assert group.used
    group.edges.forEach (x, y, c) ->
      g2 = groups.get x, y, c
      assert g2.used
      connections.add group, g2

    complete.add group

  get: (group) ->
    if !complete.has group
      findConnections group
    return connections.getAll group

  check: (invasive) ->
    if invasive
      groups.forEach (g) =>
        set = @get g
        assert set

    connections.forEach (group1, group2) ->
      if invasive
        assert complete.has group1
        assert complete.has group2

      return unless complete.has group1
      #log 'gggg', group1
      assert group1.used
      assert group2.used

      found = no
      group1.edges.forEach (x, y, c) ->
        found = yes if group2.points.has x, y, c
      assert found

  checkEmpty: ->
    assert.equal 0, connections.size



# A region is a set of connected groups where all the groups are only connected
# in a single set of shuttle states. For example, a tunnel with a shuttle in it
# will have regions to connect the groups on either side of the shuttle.
#
# *******
# *1111S*
# *******
# but
# *******
# *22S33*
# *******
#
# Regions only connect groups which have the same shuttle key. That is, groups
# which depend on exactly the same set of shuttle states.
Regions = (fillKeys, groups, groupConnections) ->
  # The region grid is lovely. This maps from:
  # group -> set of shuttle states -> region
  regionsForGroup = new Map
  regionsForGroup.default = (g) -> new util.ShuttleStateMap g.shuttles
    #shuttles = util.uniqueShuttlesInStates(fillKeys.getFilledStates g.fillKey)

  # This is needed for cleanup. Regions are destroyed when an adjacent group is
  # destroyed so if the group should now connect, we can take that into
  # account.
  regionsTouchingGroup = new Map
  regionsTouchingGroup.default = -> new Set

  regions = new Set

  watch = new Watcher

  groups.deleteWatch.on (group) ->
    map = regionsForGroup.get group
    return unless map
    regionsForGroup.delete group

    map.forEachValue (region) -> deleteRegion region

    set = regionsTouchingGroup.get group
    if set
      set.forEach (region) -> deleteRegion region
      regionsTouchingGroup.delete group

  deleteRegion = (region) ->
    log 'delete region', region._id
    assert region.used
    region.used = no
    regions.delete region
    region.groups.forEach (group) ->
      regionsForGroup.get(group)?.delete region.states
    region.edges.forEach (group) ->
      if (set = regionsTouchingGroup.get group)
        set.delete region
        regionsTouchingGroup.delete(group) if set.size is 0
    watch.signal region

  Region = (group0, trimmedStates, shuttleStateMap) ->
    assert regionsForGroup.getDef(group0).isDefinedFor shuttleStateMap

    shuttleKey = group0.shuttleKey

    @_id = makeId()
    @used = yes
    @size = 0
    @groups = new Set
    @states = trimmedStates # Set of required states for use
    @edges = new Set # Set of adjacent groups
    @engines = new Set # Hoisted from groups

    log @_id, ': createRegion from group', group0._id#, shuttleStateMap

    # Most cells (and hence most groups) have an empty fill key - I should
    # totally be able to short circuit most of this, or just use the group in
    # place of the region or something.

    util.fillGraph group0, (group, hmm) =>
      # There's three reasons we won't connect a region across group lines:
      # 1. We don't connect (in which case this won't be called)
      # 2. The other group depends on more (or less) shuttles
      if group.shuttleKey != shuttleKey
        @edges.add group
        regionsTouchingGroup.getDef(group).add this
        return

      # 3. The other group is filled in one of my states
      filledStates = fillKeys.getFilledStates group.fillKey
      filled = no
      trimmedStates.forEach (state) ->
        filled = yes if filledStates.has state
      return if filled

      # Ok, we're looking good.
      regionsForGroup.getDef(group).set trimmedStates, this
      @size++
      @groups.add group
      group.engines.forEach (e) => @engines.add e

      groupConnections.get(group)?.forEach hmm

    assert @size
    regions.add this
    log @_id, ': Made region with groups', @groups.map((g) -> {id:g._id, points:g.points})
    return

  makeRegion = (group, shuttleStateMap) ->
    # First we need to make sure the region is actually fillable given the
    # current set of shuttle states.

    # A map of just the states that are referenced by the group
    trimmedStates = new Map

    # Check to see if this group is filled in the specified states
    invalid = no
    filledStates = fillKeys.getFilledStates group.fillKey
    group.shuttles.forEach (s) ->
      state = shuttleStateMap.get s
      trimmedStates.set s, state
      invalid = yes if filledStates.has state

    # We were asked to create a region for a group with shuttle states that
    # fill the group.
    if invalid
      # Null = this group is filled here; no region is possible.
      #regionsForGroup.getDef(group0).set shuttleStateMap, null
      #log '-> invalid'
      regionsForGroup.getDef(group).set shuttleStateMap, null
      return null

    return new Region group, trimmedStates, shuttleStateMap

  #dependancies: (group) -> regionsForGroup.getDef(group).shuttles

  watch: watch

  get: (group, shuttleStateMap) ->
    # The existance of this function taking in shuttleStateMap is the only
    # reason we maintain the state map. ... .... :/
    map = regionsForGroup.getDef group
    region = map.get shuttleStateMap
    region = makeRegion group, shuttleStateMap if region is undefined

    return null if region is null # The group is filled right now.

    return region

  check: ->
    regions.forEach (r) ->
      #log 'check', r
      assert r.used
      assert r.size
      r.groups.forEach (g) ->
        assert g.used

  checkEmpty: ->
    assert.equal 0, regionsForGroup.size
    assert.equal 0, regionsTouchingGroup.size
    assert.equal 0, regions.size


CurrentStates = (shuttles, stateForce, shuttleStates) ->
  # This stores & propogates the shuttle's current states.
  # Current state is now stored on the shuttle (shuttle.currentState)
  # ... but we're also storing it here so we can index using the map in
  # regions. I'm not sure if thats actually a good idea.
  currentStates = new Map # shuttle -> state.

  # ... So, we don't want to tell everyone the current states have changed as
  # soon as we move them - other shuttles might depend on the zones where they
  # are. We'll hold any changes to the state map until step() is done then send
  # them all.

  watch = new Watcher

  shuttles.addWatch.forward (s) ->
    state = shuttleStates.getInitialState s
    s.currentState = state
    currentStates.set s, state
    watch.signal s, null, state # prev state null in the signal.

  shuttles.deleteWatch.on (s) ->
    # .. I'll try to clean up if you edit the grid mid-step(), but ... don't.
    currentStates.delete s
    # This is important so in the shuttle collide code we don't double-collide
    # a shuttle (if we just delete it in patch, its currentState will revert
    # and it might collide with something else).
    s.currentState = null
    # If you care about the current state going away, watch shuttles.deleteWatch.

  _flush = (shuttle) ->

  map: currentStates
  watch: watch

  set: (shuttle, state) ->
    assert.strictEqual state.shuttle, shuttle

    return if shuttle.currentState == state
    log "moving #{shuttle.id} to #{state.dx},#{state.dy}"

    prevState = shuttle.currentState
    shuttle.currentState = state
    currentStates.set shuttle, state
    watch.signal shuttle, prevState, state


CollapseDetector = (grid, shuttleBuffer, shuttles, shuttleStates, shuttleGrid) ->
  # This is a simple stateless module which deletes shuttles when dangerous stuff happens.
  # I split it out of ShuttleGrid for modularity's sake.
  #
  # This has nothing to do with merging shuttles when they touch. Never did.

  # Delete the shuttle when the base grid changes around it
  grid.beforeWatch.forward (x, y, oldv, v) ->
    # We only care if a cell that was previously passable became inpassable.
    oldPassable = letsShuttleThrough(oldv)
    newPassable = letsShuttleThrough(v)

    if !oldPassable and newPassable
      # We need to delete any states which are not valid so they can get regenerated
      shuttleGrid.stateGrid.get(x, y)?.forEach (state) ->
        shuttleStates.delete state if !state.valid

    else if oldPassable and !newPassable
      shuttleGrid.stateGrid.get(x, y)?.forEach (state) ->
        shuttle = state.shuttle

        # So, two strategies for dealing with this.
        if shuttle.currentState is state
          # Option 1: If the base was edited underneath the current state, game
          # over. Just bake the shuttle back onto the buffer in its current state
          # and let nature take its course.
          shuttles.delete shuttle, state
        else
          # Option 2: If the state isn't the current state, some set of the shuttle
          # states might no longer be reachable. I'll delete all states which
          # aren't the current state.
          shuttleStates.collapse shuttle

  shuttleBuffer.watch.on (x, y, v) ->
    # I don't care about collapsing shuttles in the path or anything - the
    # normal overlap / adjacency detection code will take care of that.
    shuttle = shuttleGrid.getShuttle x, y
    if shuttle
      shuttles.delete shuttle, shuttle.currentState

    # Also collapse any adjacent shuttle that needs welding.
    for {dx, dy},d in DIRS when shuttleConnects v, d
      if (shuttle = shuttleGrid.getShuttle x+dx, y+dy)
        shuttles.delete shuttle, shuttle.currentState

Zones = (shuttles, regions, currentStates) ->
  # A zone is a set of regions which are connected because of the current
  # shuttle states.  These are constantly destroyed & regenerated as shuttles
  # move around.

  #zones = new Set # This set may be redundant.
  zoneForRegion = new Map # region -> zone

  # For garbage collection. If the state of any of these shuttles changes, the
  # zone must be destroyed.
  zonesDependingOnShuttle = new WeakMap # shuttle -> set of zones
  zonesDependingOnShuttle.default = -> new Set

  watch = new Watcher

  buffering = false
  buffer = []

  regions.watch.on (r) ->
    deleteZone zoneForRegion.get r
    zoneForRegion.delete r

  deleteZonesWithShuttle = (shuttle) ->
    log 'deleteZonesWithShuttle', shuttle.id
    # Note that zoneForRegion doesn't get cleared here. We'll just leave an old
    # zone kicking around assuming we'll replace it soon anyway.
    zonesDependingOnShuttle.get(shuttle)?.forEach (zone) ->
      deleteZone zone

  shuttles.deleteWatch.on deleteZonesWithShuttle
  currentStates.watch.on deleteZonesWithShuttle

  deleteZone = (z) ->
    return unless z?.used
    log 'deleting zone', z._id
    z.used = false
    #zones.delete z
    if buffering
      buffer.push z
    else
      watch.signal z

  makeZone = (r0) ->
    # Make a zone starting from the specified region.
    zone =
      _id: makeId() # For debugging
      used: true
      pressure: 0
      fixed: true
      filled: no

      # _debug_regions: new Set # For debugging.

    log zone._id, ': makezone from', r0?._id

    # Set of engines inside the zone
    engines = new Set

    if r0 then util.fillGraph r0, (r, hmm) ->
      log 'zone fillGraph', r._id #, zoneForRegion.get(r)
      assert !zoneForRegion.get(r)?.used
      zoneForRegion.set r, zone
      # zone._debug_regions.add r

      zone.fixed = false if r.states.size
      r.states.forEach (state) ->
        #dependancies.add state.shuttle
        zonesDependingOnShuttle.getDef(state.shuttle).add zone

      r.engines.forEach (e) ->
        if !engines.has e
          assert e.used
          engines.add e
          zone.pressure += e.pressure

      # And recurse.
      r.edges.forEach (group) ->
        assert group.used
        r = regions.get group, currentStates.map
        if r is null
          # We were stymied because the group is filled at the moment. The
          # group could be filled by any one of the shuttles in group.shuttles
          # - we could go looking to find which one, but for now I'm just going
          # to add them all as dependancies.
          zonesDependingOnShuttle.getDef(shuttle).add zone for shuttle in group.shuttles
        # r is null if the group is currently filled.
        hmm r if r


    #zones.add zone
    zone

  startBuffer: ->
    buffering = true
  flushBuffer: ->
    for z in buffer
      watch.signal z
    buffer.length = 0
    buffering = false

  watch: watch

  makeZoneUnderShuttle: (shuttle) ->
    # This is used when two shuttles abut one another. We need to wake the
    # shuttle if its adjacent shuttle moves. (smallturtle test)
    # For that we'll use a sort of dummy zone that doesn't contain any regions,
    # but which will be wiped when the dependant shuttle moves.
    #
    # Could reuse these, but it probably doesn't matter for most machines (?)
    zone = makeZone null
    zonesDependingOnShuttle.getDef(shuttle).add zone
    zone.filled = true
    return zone

  getZoneForRegion: (region) ->
    assert region
    zone = zoneForRegion.get region
    zone = makeZone region if !zone?.used
    return zone

  getZoneForGroup: (group) ->
    r = regions.get group, currentStates.map
    # We'll return null if there's no zone here, because the group is filled by
    # a shuttle.
    return if r then @getZoneForRegion r else null

  checkEmpty: ->
    assert.strictEqual 0, zoneForRegion.size


AwakeShuttles = (shuttles, shuttleStates, stateForce, currentStates, zones) ->
  # This keeps track of shuttles which might move on the next step. Shuttles
  # sleep when they've been stationary for 1 step, and its impossible for them
  # to move on the next step too.
  #
  # We figure out what would wake a shuttle up during step() based on *why* the
  # shuttle didn't move. The contributors are:
  # - Zones next to a shuttle. Zones being deleted always wake their adjacent
  # shuttles.
  # - A blocking shuttle. If a shuttle blocks another shuttle from moving (or
  # decreases its imulse at all) then the blocked shuttle will be woken up when
  # the blocking shuttle wakes up. (The blocking shuttle might move this step,
  # or have a different impulse, or be movable. So we have to check its
  # blockers).

  # Shuttles are either asleep or awake. Awake shuttles don't need any extra
  # metadata - they're just marked in the awake list and iterated during
  # step().
  awake = new Set # set of shuttles

  # Ugh, this is way more complicated than it needs to be.
  #
  # If a zone is destroyed, we need a list of all the shuttles which need to be
  # woken up. If a shuttle gets woken up, we need to remove that list.
  shuttlesForZone = new Map # Zone -> Set of shuttles
  shuttlesForZone.default = -> new Set
  #shuttleZoneDeps = new Map # Shuttle -> set of zones. For gc.
  #shuttleZoneDeps.default = -> new Set

  shuttles.deleteWatch.on (s) ->
    log 's deletewatch', s

    # wake clears most of the state associated with a shuttle.
    # No need to repeat work - we'll wake it then remove it.
    wake s
    awake.delete s

  zones.watch.on (z) ->
    log 'zw', z._id, z
    # A zone was deleted. Wake any relevant shuttles.
    if (set = shuttlesForZone.get z)
      log 'zones watch', z._id
      set.forEach (s) -> wake s, 'adjacent zone killed'
      shuttlesForZone.delete z

  stateForce.watch.on (state) ->
    # The force on the shuttle was changed in some way.
    wake state.shuttle, 'force recalculated' if state.shuttle.currentState is state

  # The shuttle moved. Wake that sucker for the next step too.
  currentStates.watch.on (shuttle) ->
    wake shuttle, 'shuttle state changed'

  shuttleStates.deleteWatch.on (state) ->
    log 'state deletewatch', state
    # We'll delete invalid states when the region is passable once again
    wake state.shuttle, 'state was deleted'

  clearDeps = (shuttle) ->
    zones = shuttle.zoneDeps
    zones.forEach (z) ->
      shuttlesForZone.get(z)?.delete shuttle
    zones.clear()

    # This is unnecessary when called from wake().
    shuttle.shuttleDeps.forEach (s2) -> s2.shuttleDeps.delete shuttle

    shuttle.shuttleDeps.clear()

  wake = (shuttle, reason) -> # reason is just a string for logging.
    # This function is also called when the shuttle is deleted by both
    # stateForce.watch and currentStates.watch. And that'll happen before the
    # shuttle is actually deleted by shuttles.deleteWatch.
    #
    # In those cases we'll wake it to purge the metadata then delete it from
    # the awake set.
    
    if awake.has shuttle
      log 'redundant wake', shuttle.id, reason if reason
      return

    log "waking #{shuttle.id} because #{reason}"

    # Order of these next lines is important. Since the dependancy graph is
    # bijective, we'll do a depth first search through the graph. Cycles
    # mustn't ruin us.
    awake.add shuttle
    shuttle.shuttleDeps.forEach (s2) -> wake s2, 'shuttle dependancy moved'
    clearDeps shuttle

  data: awake

  # The shuttle is asleep - but it will wake if these zones change or blocking
  # shuttles move.
  sleep: (shuttle) ->
    return if !awake.has shuttle

    # An awake shuttle didn't move. Put it to sleep.
    awake.delete shuttle

    zones = shuttle.zoneDeps
    actuallyAsleep = true
    zones.forEach (z) -> if !z.used
      actuallyAsleep = false

    #if !actuallyAsleep
      # This is kind of awful. The problem is that during step() we calculate
      # new forces. That in turn calculates new shuttle states, which
      # calculates new groups, regions and finally zones. So shuttles sometimes
      # need to be immediately woken up again because their environment was
      # ripped apart.
      #
      # Thankfully it'll only happen during the warmup. We'll sleep the shuttle
      # then immediately wake it again to make sure all the metadata is
      # restored correctly.
      #log "NOT SNOOZING #{shuttle.id} - zones changed"
      #wake shuttle, 'zones already changed under the shuttle'
      #return

    zones.forEach (z) ->
      shuttlesForZone.getDef(z).add shuttle

    log 'setAsleepDeps', shuttle.id

  forEach: (fn) ->
    shuttles.flush()
    awake.forEach fn

  isAwake: (shuttle) -> awake.has shuttle

  check: ->
    # Awake shuttles aren't listed in shuttle zone deps.
    awake.forEach (s) ->
      assert.strictEqual s.zoneDeps.size, 0
      assert.strictEqual s.shuttleDeps.size, 0

    # All zones we're listening on should be used
    shuttlesForZone.forEach (shuttles, zone) ->
      assert zone.used
      # And shuttle.zoneDeps and shuttlesForZone should be a bijection.
      shuttles.forEach (s) ->
        assert s.used
        assert !awake.has(s)
        assert s.zoneDeps.has zone

    shuttles.forEach (s) ->
      s.zoneDeps.forEach (z) ->
        assert shuttlesForZone.get(z).has s
      s.shuttleDeps.forEach (s2) ->
        assert s2.shuttleDeps.has s

  checkEmpty: ->
    assert.strictEqual awake.size, 0

  stats: ->
    console.log 'shuttlesForZone.size:', shuttlesForZone.size
    #console.log 'shuttleZoneDeps.size:', shuttleZoneDeps.size

ShuttleOverlap = (shuttleStates, shuttleGrid) ->
  # Pairs of states in different shuttles which would overlap
  overlappingStates = new SetOfPairs

  shuttleStates.addWatch.forward (state1) ->
    return unless state1.valid
    state1.shuttle.points.forEach (x, y) ->
      shuttleGrid.stateGrid.get(x+state1.dx, y+state1.dy)?.forEach (state2) ->
        return if state2.shuttle is state1.shuttle
        #log 'overlapping states', state1, state2
        overlappingStates.add state1, state2

  shuttleStates.deleteWatch.on (state) ->
    overlappingStates.deleteAll state

  forEach: (state1, fn) ->
    overlappingStates.getAll(state1)?.forEach (state2) ->
      fn state2, state2.shuttle # if state2.shuttle.immState is state2


Step = (modules) ->
  #logg = debug 'step'

  {
    zones,
    shuttles,
    awakeShuttles,
    shuttleStates,
    stateForce,
    currentStates,
    shuttleOverlap,
    fillKeys,
  } = modules

  # Alright, the rules are something like this:
  #
  # For each awake shuttle:
  #
  # 0. If the shuttle has already moved in an orthogonal direction, discard impulse.
  # 1. If the space a shuttle is moving into is empty, move the shuttle
  # 2. If there is a wall in front of the shuttle, keep impulse but continue.
  # 3. If there is 1 or more shuttles in front of the shuttle, calculate the transitive
  #   blockers for moving in the chosen direction. Sort.
  #   - For each point of pressure opposing the shuttle, cancel out the shuttle's impulse
  #   - (impulse -> 0, stop)
  #   - If the transitive shuttles can be pushed, push them.
  #   - For each unit moved, for each point of impulse in the *same* direction, cancel 1 when
  #     the shuttle moves.
  #
  #
  # Whenever a shuttle moves:
  # - Mark its shadow as being impassable from orthogonal directions
  # - Wake it
  # - Move it
  # - Consume impulse in the direction of travel, if there is any (even if pushed)
  #
  # Anything that runs down a shuttle's impulse should wake it up when its pressure changes.


  tag = 0 # Incremented at the start of each step.

  # Awake shuttles which have left-right impulse or up-down impulse. Cleared at
  # the end of step().
  shuttlesX = []
  shuttlesY = []

  # Given a force, calculate the resultant impulse (pressure) on the shuttle.
  calcImpulse = (shuttle, f) ->
    return 0 if !f

    impulse = 0

    f.forEach (mult, group) ->
      #log 'calculating pressure in group', group
      assert group.used
      zone = zones.getZoneForGroup group
      if zone
        assert zone.used
        log 'pressure', zone.pressure if zone.pressure

        # note `-` because we're calculating it from the POV of the zone.
        impulse -= mult * zone.pressure
      else
        # The zone is filled by a shuttle. It won't contribute any pressure,
        # but we need to figure out which shuttle is blocking us so when that
        # shuttle moves we get woken up.

        # (We could probably pass this back from regions.get to avoid this
        # calculation, but its probably not a big deal.)
        filledStates = fillKeys.getFilledStates group.fillKey

        blockingShuttle = null
        group.shuttles.forEach (s) ->
          if filledStates.has s.currentState
            assert.equal blockingShuttle, null # Exactly 1 shuttle here.
            blockingShuttle = s
        assert blockingShuttle

        # So, this is a total hack. We need to wake this shuttle up the step
        # after the adjacent shuttle *moves*. I could reuse the codepath for
        # waking a shuttle when a pushing shuttle has its pressure changed, but
        # thats a bit over-eager. Mind you, those events will often happen
        # together anyway so it might not be a big deal. But I wrote this code
        # first, and behaviour is better so I'm keeping it for now.
        zone = zones.makeZoneUnderShuttle blockingShuttle

      shuttle.zoneDeps.add zone

    return impulse


  tryMove = (shuttle, isTop) ->
    newTag = if isTop then tag else -tag
    # The shuttle has moved in the orthogonal direction this step. Discard.
    return false if shuttle.stepTag is -newTag

    moved = no
    im = if isTop then shuttle.imYRem else shuttle.imXRem
    return false if !im # Happen if the pressure is consumed by opposing forces.

    mul = 1
    dir = if im < 0
      im = -im
      mul = -1
      if isTop then UP else LEFT
    else
      if isTop then DOWN else RIGHT

    log 'tryMove shuttle', shuttle.id, util.DN[dir], im

    # TODO: Make this null when there's no shuttle as an optimization.
    shuttleList = [shuttle] # Kept sorted because iteration order is important.
    needSort = false
    # TODO: Optimise so the normal case (only one shuttle) doesn't need this allocation.

    shuttleGlob = new Set # Includes all shuttles that we're pushing. Used to dedup shuttleList.
    shuttleGlob.add shuttle

    while im
      assert im > 0

      blocked = false

      # Usually there's nothing in the way, and the shuttle just gets moved.
      #
      # But most of this code exists in case there are *shuttles* in the way!
      # Uh oh!
      #
      # We're going to glob up the whole bunch of movable shuttles and bring
      # them for the ride.
      #
      # There's 3 parts to that:
      # 1. Get all the shuttles to push. As we go we check that they can move
      #    (the next state is valid).
      # 2. Calculate pressure (so, if they have impulse resisting, decrease.)
      # 3. If we still have impulse left over, push them all. Consume
      #    contributing forces.


      # 1. Gather all the shuttles that are about to be pushed.
      oppositingForce = 0
      i = 0
      while i < shuttleList.length
        s = shuttleList[i]

        # The next state the shuttle will be in if it moves
        nextState = shuttleStates.getStateNear s.currentState, dir

        # 1. If a shuttle has hit a wall, a shadow, or a shuttle that can't
        # move then stop.
        if !nextState #|| nextState.stepTag == -newTag
          log 'shuttle hit wall in dir', util.DN[dir], 'from state', s.currentState.dx, s.currentState.dy
          blocked = true
          break # damn I want a multilevel break here.

        shuttleOverlap.forEach nextState, (state2, s2) ->
          if state2.stepTag == -newTag # Hit a shadow
            log 'Blocked by shadow'
            return blocked = true

          return if s2.currentState != state2 # Shuttle not actually here.

          # hit a shuttle that has already moved orthogonally.
          if s2.stepTag == -newTag
            log 'blocked by shuttle'
            return blocked = true

          # We've hit another shuttle that we can push.
          if !shuttleGlob.has s2
            shuttleGlob.add s2
            shuttleList.push s2
            needSort = true

            im2 = -mul * if isTop then s2.imYRem else s2.imXRem
            oppositingForce += im2 if im2 > 0

        i++

      break if blocked

      # 2. Calculate pressure, consuming opposing pressure as we go.
      if oppositingForce > im
        log 'Opposing force too great', oppositingForce, im
        # Ok, the shuttles are pushing back so hard we can't move. Consume
        # impulse from them in their magic order.
        shuttleList.sort compareByPosition if needSort
        needSort = false

        for s2 in shuttleList
          im2 = -mul * if isTop then s2.imYRem else s2.imXRem
          if im2 > 0
            take = Math.min im, im2
            assert take > 0

            assert awakeShuttles.isAwake s2
            # We're stealing some force from this shuttle. If either of us
            # doesn't move, its because of this moment.
            shuttle.shuttleDeps.add s2
            s2.shuttleDeps.add shuttle

            im2 -= take; im -= take
            if isTop then s2.imYRem = -im2*mul else s2.imXRem = -im2*mul
            break if im is 0

      else if oppositingForce > 0
        # Crush all resistance!
        log 'Crushing resistance force of', oppositingForce
        im -= oppositingForce
        for s2 in shuttleList
          im2 = -mul * if isTop then s2.imYRem else s2.imXRem
          if im2 > 0
            assert awakeShuttles.isAwake s2
            shuttle.shuttleDeps.add s2
            s2.shuttleDeps.add shuttle
            if isTop then s2.imYRem = 0 else s2.imXRem = 0

      break if im is 0
      assert im > 0

      # 3. Push them all!
      im--
      for s2 in shuttleList
        nextState = shuttleStates.getStateNear s2.currentState, dir
        assert nextState

        # Tag the shuttle as having moved
        s2.stepTag = newTag
        # Tag the old state as being impassable from ortho directions
        s2.currentState.stepTag = newTag

        log 'Moving shuttle', s2.id, 'to state', nextState.dx, nextState.dy
        
        currentStates.set s2, nextState
        moved = yes
        if isTop then s2.imXRem = 0 else s2.imYRem = 0

        # Clip the wings a little. Note that this will happen to the main pushing shuttle too.
        im2 = mul * if isTop then s2.imYRem else s2.imXRem
        if im2 > 0
          im2--
          if isTop then s2.imYRem = im2*mul else s2.imXRem = im2*mul

      moved = yes

    if isTop then shuttle.imYRem = im*mul else shuttle.imXRem = im*mul
    log 'tryMove ->', moved
    return moved


  # Step the world!
  # returns true if something moves.
  return step = ->
    tag++
    log "************** STEP #{tag} **************"
    log '** phase 1) calculating pressure ***'

    zones.startBuffer()
    # Part 1: Calculate the impulse on all shuttles.
    #shuttles.forEach (shuttle) ->
    awakeShuttles.forEach (shuttle) ->
      assert.equal shuttle.zoneDeps.size, 0
      assert.equal shuttle.shuttleDeps.size, 0

      log 'step() looking at shuttle', shuttle.id
      Jit.stats.checks++

      return if shuttle.held # Manually set from the UI.

      # Consider moving the shuttle.
      assert shuttle.used
      force = stateForce.get shuttle.currentState
      {x:fx, y:fy} = force
      #log 'step() looking at shuttle', shuttle.id, 'force', force

      if (im = calcImpulse shuttle, fx)
        shuttle.imXRem = shuttle.imX = im
        shuttlesX.push shuttle
      if (im = calcImpulse shuttle, fy)
        shuttle.imYRem = shuttle.imY = im
        shuttlesY.push shuttle

      log 'impulse', shuttle.imX, shuttle.imY

    # We're going to sort the shuttles we want to move so that the simulation
    # is stable.
    #
    # Its super gross copying this code, but the other way would be to lookup
    # by key 'imX' and 'imY' and that'd be slow (I think). Eh.
    shuttlesX.sort (a, b) ->
      impulseDiff = abs(b.imX) - abs(a.imX)
      if (impulseDiff) then return impulseDiff
      else return compareByPosition a, b
    shuttlesY.sort (a, b) ->
      impulseDiff = abs(b.imY) - abs(a.imY)
      if (impulseDiff) then return impulseDiff
      else return compareByPosition a, b


    # Part 2: Try and move all the shuttles.
    log '***** phase 2) moving shuttles *****'
    log 'Shuttles to move: ', shuttlesX.map((s) -> s.id), shuttlesY.map((s) -> s.id)

    numMoved = 0

    # Walk along shuttlesX and shuttlesY together, taking whichever has more
    # impulse.
    ix = iy = 0
    loop
      if ix == shuttlesX.length
        numMoved += tryMove s, true for s in shuttlesY[iy...]
        break
      if iy == shuttlesY.length
        numMoved += tryMove s, false for s in shuttlesX[ix...]
        break

      # Note > not >=.
      if abs((sx = shuttlesX[ix]).imX) > abs((sy = shuttlesY[iy]).imY)
        ix++; numMoved += tryMove sx, false
      else
        iy++; numMoved += tryMove sy, true
    
    # Actually forward signals based on shuttles that moved, to kill zones and
    # figure out the new set of dirty states for the next step. Note that this
    # *needs* to happen after the shuttles are slept, so they can be re-awoken
    # if a zone they depend on has been bumped.

    log '**** phase 3) cleanup ****'

    # Sleep all the shuttles that haven't moved. Note that this isn't perfect -
    # if a shuttle moves then gets pushed back to its original position, it'll
    # stay awake. Its a pretty rare case and everything is simpler that way.
    awakeShuttles.forEach (shuttle) ->
      if abs(shuttle.stepTag) == tag
        Jit.stats.moves++
        log 'shuttle', shuttle.id, 'still awake - ', shuttle.stepTag
        # The shuttle will stay awake. Clear its dependancies.
        shuttle.zoneDeps.clear()
        shuttle.shuttleDeps.forEach (s2) -> s2.shuttleDeps.delete shuttle
        shuttle.shuttleDeps.clear()
      else
        log 'sleeping shuttle', shuttle.id, Array.from(shuttle.zoneDeps).map((z) -> z._id)
        awakeShuttles.sleep shuttle

    zones.flushBuffer()

    log 'moved', numMoved
    shuttlesX.length = shuttlesY.length = 0
    return !!numMoved
 

    calcPressure()
    return update()


module.exports = Jit = (rawGrid) ->
  baseGrid = BaseGrid()

  engineBuffer = BaseBuffer baseGrid, ['positive', 'negative']
  engines = BlobFiller 'engine', engineBuffer
  engineGrid = EngineGrid baseGrid, engines

  shuttleBuffer = ShuttleBuffer()
  shuttles = BlobFiller 'shuttle', shuttleBuffer

  shuttleStates = ShuttleStates baseGrid, shuttles
  shuttleGrid = ShuttleGrid shuttleStates
  CollapseDetector baseGrid, shuttleBuffer, shuttles, shuttleStates, shuttleGrid

  fillKeys = FillKeys baseGrid, shuttleStates, shuttleGrid
  groups = Groups baseGrid, engines, engineGrid, shuttleGrid, fillKeys
  stateForce = StateForce baseGrid, shuttleStates, shuttleGrid, groups
  currentStates = CurrentStates shuttles, stateForce, shuttleStates
  groupConnections = GroupConnections groups
  regions = Regions fillKeys, groups, groupConnections
  zones = Zones shuttles, regions, currentStates

  shuttleOverlap = ShuttleOverlap shuttleStates, shuttleGrid, currentStates
  awakeShuttles = AwakeShuttles shuttles, shuttleStates, stateForce, currentStates, zones

  modules = {baseGrid, engineBuffer, engines, engineGrid, shuttleBuffer,
    shuttles, shuttleStates, shuttleGrid, fillKeys, groups, stateForce,
    groupConnections, regions, currentStates, zones, awakeShuttles,
    shuttleOverlap}

  step = Step modules

  set = (x, y, bv, sv) ->
    baseGrid.set x, y, bv
    shuttleBuffer.set x, y, sv

  setGrid = (rawGrid) -> util.deserialize rawGrid, no, set

  setGrid rawGrid



  baseGrid: baseGrid
  modules: modules


  getZoneContents: (x, y, c) ->
    # This is used by the UI to shade the region the mouse is over.
    group = modules.groups.get x, y, c
    return null unless group

    points = new Set2 # Set3 instead?
    engines = new Set

    r0 = modules.regions.get group, modules.currentStates.map
    if r0 then util.fillGraph r0, (r, hmm) ->
      r.groups.forEach (g) ->
        g.points.forEach (x, y, c, v) -> points.add x, y
      r.engines.forEach (e) -> engines.add e
      r.edges.forEach (group) ->
        assert group.used
        if (r = modules.regions.get group, modules.currentStates.map)
          hmm r

    return {points, engines}


  # Try to move the named shuttle to the specified state immediately.
  #
  # Be careful calling this while we're in step().
  moveShuttle: (shuttle, state) ->
    overlap = false
    shuttleOverlap.forEach state, (state2, shuttle2) ->
      overlap = true if shuttle2.currentState is state2
    # TODO: push the pesky shuttles out of the way using the monster code in step.
    if !overlap
      currentStates.set shuttle, state

  step: ->
    result = step()
    return result

  check: (invasive) ->
    m.check?(invasive) for k, m of modules

    # Each shuttle should be on top of nothing
    shuttles.forEach (shuttle) ->
      shuttle.eachCurrentPoint (x, y, v) ->
        baseV = baseGrid.get x, y
        assert baseV in ['nothing', 'bridge', 'ribbon', 'ribbonbridge']

  checkEmpty: ->
    m.checkEmpty?() for k, m of modules

  printGrid: (stream = process.stdout) ->
    overlay = new Map2
    ids = new Map2
    shuttles.forEach (s) ->
      state = s.currentState
      if !state
        return log 'no state for', s
      {dx, dy} = state
      s.points.forEach (x, y, v) ->
        overlay.set x+dx, y+dy, if (v & SHUTTLE) then 'shuttle' else 'thinshuttle'
        ids.set x+dx, y+dy, s.id

    util.printCustomGrid util.gridExtents(baseGrid), (x, y) ->
      overlay.get(x, y) or baseGrid.get x, y
    , ids.get.bind(ids), stream


  toJSON: ->
    json =
      base: {}
      shuttles: {}

    baseGrid.forEach (x, y, v) ->
      assert typeof v is 'string'
      json.base["#{x},#{y}"] = v if v?

    shuttles.forEach (s) ->
      {dx, dy} = state = s.currentState
      s.points.forEach (x, y, v) ->
        json.shuttles["#{x+dx},#{y+dy}"] = v

    #console.log json
    json

  set: set # set(x, y, bv, sv)
  get: (layer, x, y) ->
    switch layer
      when 'shuttles' then shuttleBuffer.data.get(x, y) or shuttleGrid.getValue x, y
      when 'base' then baseGrid.get x, y
      else throw Error "No such layer #{layer}"

  stats: ->
    console.log Jit.stats
    m.stats?() for k, m of modules
    return

  setQuiet: (v = no) -> log.quiet = v

Jit.stats =
  moves: 0
  checks: 0

# A day may come when I need this, but it is not this day.
# Jit.Buffer = BaseBuffer

parseFile = exports.parseFile = (filename, opts) ->
  fs = require 'fs'
  data = JSON.parse fs.readFileSync(filename, 'utf8').split('\n')[0]
  # Mmmm, resiliancy.
  delete data.tw
  delete data.th
  jit = new Jit data, opts

  jit.modules.shuttles.forEach (s) ->
    console.log "Shuttle #{s.id} has points", s.points

  jit.printGrid()
  #util.printGrid util.gridExtents(jit.grid), jit.grid

  for s in [1..10]
    moved = jit.step()
    console.log 'Post step', s
    jit.printGrid()

    console.log '# Awake shuttles:', Array.from(jit.modules.awakeShuttles.data).map((s) -> s.id)
    console.log()

    if !moved # If we're stuck make sure its for real.
      log.quiet = true
      json = jit.toJSON()
      j2 = new Jit json
      assert !j2.step(), 'World erroneously stable'

      console.log '-> World stable.'
      break

    #jit.modules.shuttles.forEach (s) =>
    #  if !jit.modules.awakeShuttles.data.has s
    #    console.log 'b', s.id, s.blockedX?.id, s.blockedY?.id

  log '-----'
  #jit.grid.set 5, 2, null

if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  log.quiet = process.argv[3] != '-v'
  parseFile filename
  console.log Jit.stats
  console.log "(#{Math.floor(100 * Jit.stats.moves / Jit.stats.checks)}% efficiency)"

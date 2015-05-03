# This jit is designed as an ecosystem of signaling parts.
Watcher = require './watch'
{Map2, Map3, Set2, Set3, SetOfPairs} = require './collections2'
{parseXY, fill, DIRS} = util = require './util'
log = require './log'
assert = require 'assert'
mersenne = require 'mersenne'

UP=0; RIGHT=1; DOWN=2; LEFT=3

makeId = do ->
  nextId = 1
  -> nextId++

mersenne.seed 1234
randomWeighted = (arr) ->
  totalWeight = 0
  totalWeight += arr[i+1] for _, i in arr by 2
  ->
    r = mersenne.rand() % totalWeight
    for v,i in arr by 2
      r -= arr[i+1]
      break if r < 0

    v

letsShuttleThrough = (v) -> v in ['shuttle', 'thinshuttle', 'nothing', 'bridge']

log.quiet = yes

abs = (x) -> if x >= 0 then x else -x

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
    v = undefined if v is null
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

pump = (grid) -> (x, y) ->
  v = grid.get x, y
  grid.delete x, y if v
  return v

EngineBuffer = (grid) ->
  # EngineBuffer wraps a Map2 grid to use as a buffer between cells in the grid
  # and engines.
  #
  # When cells are turned into engines, they get deleted from the grid. Then
  # they get added back when the shuttle / engine is deleted.
  buffer = new Map2
  watch = new Watcher

  grid.afterWatch.forward (x, y, oldv, v) ->
    if oldv in ['positive', 'negative']
      # This working depends on EngineGrid, below.
      assert.equal buffer.get(x, y), oldv
      buffer.delete x, y
      watch.signal x, y

    if v in ['positive', 'negative']
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
  buffer = new Map2
  watch = new Watcher # This is a beforeWatch

  set: (x, y, v) ->
    #assert !isNaN x
    if v?
      assert v in ['shuttle', 'thinshuttle']
      return if buffer.get(x, y) is v
      watch.signal x, y, v
      buffer.set x, y, v
    else
      # Signal anyway. Not having the cell in the buffer doesn't tell us
      # anything.
      watch.signal x, y, v
      buffer.delete x, y

  watch: watch
  data: buffer
  pump: pump buffer

# The code for finding & flood filling shuttles & engines is basically
# identical. I'll reuse it, and switch according to which type of thing I'm
# looking for.
BlobFiller = (type, buffer, grid) ->
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

    blobs.add this

    # The third parameter is really just cached via the fill mechanism... but
    # it means I don't need both fill2 and fill3 in util.
    util.fill3 x, y, v0, (x, y, v, hmm) =>
      buffer.pump x, y

      @size++
      @points.set x, y, v

      for {dx, dy},d in DIRS
        x2 = x+dx; y2 = y+dy
        v2 = @points.get(x2, y2) || buffer.data.get(x2, y2)

        if v is 'shuttle' and v2 isnt 'shuttle'
          @pushEdges.add x, y, d

        if v2 and (type isnt 'engine' or v2 == v)
          hmm x2, y2, v2
        else
          @edges.add x, y, d

    assert @size
    if type is 'engine'
      @type = v0
      @pressure = (if v0 is 'positive' then 1 else -1) * @size

    log @id, ", Added #{type}", this
    addWatch.signal this
    return

  addWatch: addWatch
  deleteWatch: deleteWatch

  flush: ->
    buffer.data.forEach (x, y, v) ->
      #assert !isNaN x
      new Blob x, y, v

  flushAt: (x, y) ->
    if v = buffer.data.get x, y
      return new Blob x, y, v

  forEach: (fn) ->
    @flush()
    blobs.forEach fn

  delete: deleteBlob

  check: (invasive) ->
    @forEach(->) if invasive
    ###
    blobs.forEach (b) =>
      assert b.used
      b.points.forEach (x, y, v) =>
        assert.equal @get(x, y), b
        # - Shuttles always have all adjacent shuttle / thinshuttle points
        for {dx, dy} in DIRS when !b.points.has x+dx,y+dy
          v2 = grid.get x+dx, y+dy
          if type is 'shuttle'
            assert v2 not in ['shuttle', 'thinshuttle']
          else
            assert v2 != v
    ###

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

 

ShuttleStates = (grid, shuttles) ->
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
      fits = no if !letsShuttleThrough grid.get(x+dx, y+dy)
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

      if v is 'shuttle' and state.valid
        #log "adding #{state.id} to #{x}, #{y}"
        fillGrid.getDef(x, y).add state
        fillWatch.signal x, y

  shuttleStates.deleteWatch.on (state) ->
    log 'shuttle grid removing', state.shuttle.id, state.dx, state.dy
    state.shuttle.points.forEach (x, y, v) ->
      x += state.dx; y += state.dy

      stateGrid.get(x, y).delete state
      stateWatch.signal x, y

      if v is 'shuttle' and state.valid
        fillGrid.get(x, y).delete state
        fillWatch.signal x, y

  fillGrid: fillGrid
  fillWatch: fillWatch
  stateGrid: stateGrid
  stateWatch: stateWatch

  get: (x, y) ->
    # Get the shuttle currently at (x, y)
    shuttle = null
    stateGrid.get(x, y)?.forEach (state) ->
      if state.shuttle.currentState is state
        shuttle = state.shuttle
    return shuttle

  getValue: (x, y) ->
    # Get the grid cell value at x, y. This is a lot of work - if you want to
    # draw a lot of shuttles or something, don't use this.
    return unless shuttle = @get x, y
    {dx, dy} = shuttle.currentState
    return shuttle.points.get x-dx, y-dy

  check: ->

FillKeys = (grid, shuttleStates, shuttleGrid) ->
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

  grid.afterWatch.on (x, y, oldv, v) ->
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
    return '' if !letsShuttleThrough grid.get(x,y)
    shuttleStates.flushStatesAt x, y
    key = fillKey.get x, y
    if !key
      key = calcKeyAt x, y
      fillKey.set x, y, key
    return key

  checkEmpty: ->
    assert.equal 0, fillKey.size
    assert.equal 1, fillStates.size


Groups = (grid, engines, engineGrid, shuttleGrid, fillKeys) ->
  # The first step to calculating regions is finding similar cells. Similar
  # cells are neighboring cells which will always (come hell or high water)
  # contain the same regions.
  #
  # This is very similar to shuttles, but instead of flood filling on connected
  # shuttle cells, we'll connect cells which have the same FillGrid.

  # (x, y, cell) -> cell value. This works the same as the buffers for engines
  # and shuttles.
  pendingCells = new Set3

  groups = new Set

  # (x, y, cell) -> group
  groupGrid = new Map3

  # Needed for gc - this is a set of groups which depend on the fillgrid at x,y.
  edgeGrid = new Map2 -> new Set

  watch = new Watcher

  # For garbage collection.
  groupsWithEngine = new WeakMap
  groupsWithEngine.default = -> new Set

  deleteGroupsAt = (x, y) ->
    cmax = util.cellMax grid.get x, y
    for c in [0...cmax]
      group = groupGrid.get x, y, c
      deleteGroup group if group

  deleteGroup = (group) ->
    log group._id, ': deleting group'
    assert group.used

    # Invalidate g!
    group.used = no # More for debugging than anything.
    groups.delete group
    watch.signal group

    group.engines.forEach (e) ->
      groupsWithEngine.get(e).delete group

    # Recycle the points
    group.points.forEach (px, py, pc, pv) ->
      #log 'pcb', px, py, pc, pv
      pendingCells.add px, py, pc
      groupGrid.delete px, py, pc

    group.edges.forEach (x, y, c) ->
      edgeGrid.get(x, y).delete group

  grid.afterWatch.forward (x, y, oldv, v) ->
    cmax = util.cellMax oldv
    for c in [0...cmax]
      #log '---', x, y, c, oldv
      group = groupGrid.get x, y, c
      deleteGroup group if group
      pendingCells.delete x, y, c

    deleteGroupsAt x+dx, y+dy for {dx, dy} in DIRS

    cmax = util.cellMax v
    for c in [0...cmax]
      #log 'pca', x, y, c, v
      pendingCells.add x, y, c

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
    v0 = grid.get x, y

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
      v = grid.get x, y
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

      __g = groupGrid.get x, y, c
      log __g if __g

      assert !groupGrid.has x, y, c
      groupGrid.set x, y, c, group

      assert pendingCells.has x, y, c
      pendingCells.delete x, y, c

      if v in ['positive', 'negative']
        e = engineGrid.get x, y
        group.engines.add e
        groupsWithEngine.getDef(e).add group

      # Hmm for each adjacent cell.
      for [x2, y2, c2] in util.connectedCells grid, x, y, c
        hmm x2, y2, c2||0

      return

    groups.add group
    log group._id, ': made group', group.points unless group.useless
    assert group.size
    assert group.used
    return group

  watch: watch

  get: (x, y, c) ->
    g = groupGrid.get x, y, c

    if !g
      v = grid.get x, y
      assert 0 <= c < util.cellMax(v)

      # I don't think there's really any value in which we don't make a cell
      # group. I mean, its dumb making a group underneath an
      # immovable shuttle - but who's counting?
      if v?
        g = makeGroupAt x, y, c

    return g if !g.useless

  getDir: (x, y, dir) ->
    v = grid.get x, y
    return unless v

    c = switch v
      when 'positive', 'negative'
        dir
      when 'bridge'
        dir % 2
      else
        0

    @get x, y, c

  forEach: (fn) ->
    # This is an interesting function - more useful for debugging than anything
    # else. By fetching all groups, you'll force them all to get generated. Not
    # something you ever really want to do - but eh.
    pendingCells.forEach (x, y, c) ->
      makeGroupAt x, y, c

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

StateForce = (shuttleStates, groups) ->
  # For each shuttle state, we need to know which groups apply a force.
  stateForce = new Map # state -> force.
  stateForce.default = (state) -> makeForce state

  stateForGroup = new Map # group -> set of states
  stateForGroup.default = -> new Set
  
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

  groups.watch.on (group) ->
    #log.quiet = no if group._id is 60114
    log 'got group deleted', group._id
    if (set = stateForGroup.get group)
      set.forEach (state) -> deleteForce state

      # These should be cleaned up in deleteForce when they're empty.
      assert !stateForGroup.has group

  deleteForce = (state) ->
    if (force = stateForce.get state)
      stateForce.delete state

      log 'deleteForce', state

      force.used = no

      delGroups = (pressure, group) ->
        if (set = stateForGroup.get group)
          set.delete state
          stateForGroup.delete group if set.size is 0

      force.x?.forEach delGroups
      force.y?.forEach delGroups

  makeForce = (state) ->
    log 'makeForce', state
    assert state.shuttle.used
    assert state.valid

    canMoveX = shuttleStates.getStateNear(state, LEFT) or
      shuttleStates.getStateNear(state, RIGHT)
    canMoveY = shuttleStates.getStateNear(state, UP) or
      shuttleStates.getStateNear(state, DOWN)

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
      log 'looking in', x+dx, y+dy

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

  get: (state) ->
    f = stateForce.getDef state
    assert f.used
    f


GroupConnections = (groups) ->
  # So, here we track & report on connections between groups. Groups are either
  # complete or incomplete. Incomplete groups need to be regenerated if
  # requested, but they are useful for garbage collection.

  connections = new SetOfPairs # Map from group -> set of groups it touches
  complete = new WeakSet # Set of groups which we have all the connections of

  groups.watch.on (group) ->
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


#StateEdge = (shuttleStates) ->
  # The edge of a shuttle is a set of grid cells (x, y, cell) which may

Regions = (fillKeys, groups, groupConnections) ->
  # The region grid is lovely. This maps from:
  # group -> set of shuttle states -> region
  regionsForGroup = new Map
  regionsForGroup.default = (g) -> new util.ShuttleStateMap g.shuttles
    #shuttles = util.uniqueShuttlesInStates(fillKeys.getFilledStates g.fillKey)

  # This is needed for cleanup. Regions are destroyed when an adjacent group is
  # destroyed so if the group should now connect, we can take that into account.
  regionsTouchingGroup = new Map
  regionsTouchingGroup.default = -> new Set

  regions = new Set

  watch = new Watcher

  groups.watch.on (group) ->
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


CurrentStates = (shuttles, stateForce, shuttleStates, stepWatch) ->
  # This stores & propogates the shuttle's current states.
  # Current state is now stored on the shuttle (shuttle.currentState)
  # ... but we're also storing it here so we can index using the map in
  # regions. I'm not sure if thats actually a good idea.
  currentStates = new Map # shuttle -> state.

  # ... So, we don't want to tell everyone the current states have changed as
  # soon as we move them - other shuttles might depend on the zones where they
  # are. We'll hold any changes to the state map until step() is done then send
  # them all.
  patch = new Map

  watch = new Watcher

  shuttles.addWatch.forward (s) ->
    state = shuttleStates.getInitialState s
    s.currentState = state
    currentStates.set s, state
    watch.signal s, null, state

  shuttles.deleteWatch.on (s) ->
    # .. I'll try to clean up if you edit the grid mid-step(), but ... don't.
    patch.delete s
    currentStates.delete s
    # This is important so in the shuttle collide code we don't double-collide
    # a shuttle (if we just delete it in patch, its currentState will revert
    # and it might collide with something else).
    s.currentState = null
    # If you care about the current state going away, watch shuttles.deleteWatch.

  stepWatch.on (time) ->
    if time is 'after'
      # Call this at the end of step()
      patch.forEach (state, shuttle) ->
        log "moving #{shuttle.id} to #{state.dx},#{state.dy}"
        prev = shuttle.currentState
        shuttle.currentState = state
        currentStates.set shuttle, state
        watch.signal shuttle, prev, state
      patch.clear()
 
  map: currentStates
  watch: watch

  set: (shuttle, state) ->
    patch.set shuttle, state

  getImmediate: (shuttle) ->
    patch.get(shuttle) || shuttle.currentState

CollapseDetector = (grid, shuttleBuffer, shuttles, shuttleStates, shuttleGrid) ->
  # This is a simple stateless module which deletes shuttles when dangerous stuff happens.
  # I split it out of ShuttleGrid for modularity's sake.
 
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
    shuttle = shuttleGrid.get x, y
    if shuttle
      shuttles.delete shuttle, shuttle.currentState
    else if v
      for {dx, dy} in DIRS
        if (shuttle = shuttleGrid.get x+dx, y+dy)
          shuttles.delete shuttle, shuttle.currentState





Zones = (shuttles, regions, currentStates) ->
  # A zone is a set of regions which are connected because of the current
  # shuttle states.  These are constantly destroyed & regenerated as shuttles
  # move around.

  zones = new Set # This set may be redundant.
  zoneForRegion = new Map # region -> zone

  # For garbage collection
  zonesDependingOnShuttle = new WeakMap # shuttle -> set of zones
  zonesDependingOnShuttle.default = -> new Set

  watch = new Watcher

  regions.watch.on (r) ->
    deleteZone zoneForRegion.get r

  deleteZonesWithShuttle = (shuttle) ->
    zonesDependingOnShuttle.get(shuttle)?.forEach (zone) ->
      deleteZone zone

  shuttles.deleteWatch.on deleteZonesWithShuttle
  currentStates.watch.on deleteZonesWithShuttle

  deleteZone = (z) ->
    return unless z?.used
    log 'deleting zone', z._id
    z.used = false
    zones.delete z
    #log 'deleted zone', z
    #zoneForRegion.delete 
    z._debug_regions.forEach (r) ->
      log '-> with region', r._id
      zoneForRegion.delete r
    watch.signal z

  makeZone = (r0) ->
    # Make a zone starting from the specified group.
    zone =
      _id: makeId() # For debugging
      used: true
      pressure: 0

      _debug_regions: new Set # For debugging.

    log zone._id, ': makezone from', r0._id

    # Set of shuttles - if the state of any of these shuttles changes, the zone must die.
    #dependancies = new Set
    # Set of engines inside the zone
    engines = new Set

    util.fillGraph r0, (r, hmm) ->
      log 'zone fillGraph', r._id #, zoneForRegion.get(r)
      assert !zoneForRegion.get(r)?.used
      zoneForRegion.set r, zone
      zone._debug_regions.add r

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
          # We were stymied because the group is filled at the moment.
          # The group could be filled by any one of the shuttles in
          # group.shuttles - we could go looking to find which one, but for now
          # I'm just going to add them all as dependancies.
          zonesDependingOnShuttle.getDef(shuttle).add zone for shuttle in group.shuttles
        # r is null if the group is currently filled.
        hmm r if r


    zones.add zone
    zone


  watch: watch

  getZoneForRegion: (region) ->
    assert region
    zone = zoneForRegion.get region
    zone = makeZone region if !zone?.used
    return zone

  getZoneForGroup: (group) ->
    r = regions.get group, currentStates.map
    return if r is null then null else @getZoneForRegion r

  checkEmpty: ->
    assert.equal 0, zones.size
    assert.equal 0, zoneForRegion.size


DirtyShuttles = (shuttles, shuttleStates, currentStates, zones) ->
  # This keeps track of shuttles which might move on the next step. This includes
  # any shuttle which just moved, shuttles next to a zone which was
  # just destroyed.
  dirty = new Set # set of shuttles

  # Ugh, this is way more complicated than it needs to be.
  #
  # If a zone is destroyed, we need a list of all the shuttles which need to be
  # marked dirty.
  # If a shuttle gets marked dirty, we need to remove that list.
  shuttlesForZone = new Map # Zone -> Set of shuttles
  shuttlesForZone.default = -> new Set
  shuttleZoneDeps = new Map # Shuttle -> set of zones. For gc.
  shuttleZoneDeps.default = -> new Set

  shuttles.deleteWatch.on (s) ->
    # A shuttle was deleted.
    if (deps = shuttleZoneDeps.get s)
      deps.forEach (z) -> shuttlesForZone.get(z).delete s
      shuttleZoneDeps.delete s
    dirty.delete s

  zones.watch.on (z) ->
    #log 'zw', z._id
    # A zone was deleted. Set any relevant shuttles to dirty.
    if (set = shuttlesForZone.get z)
      #log 'zones watch', z._id
      set.forEach (s) -> setDirty s
      shuttlesForZone.delete z

  # The state changed. Make that sucker dirty.
  currentStates.watch.on (shuttle) ->
    setDirty shuttle
  shuttleStates.deleteWatch.on (state) ->
    log 'deletewatch', state
    # We'll delete invalid states when the region is passable once again
    setDirty state.shuttle

  setDirty = (shuttle) ->
    dirty.add shuttle

    if (deps = shuttleZoneDeps.get shuttle)
      # Clear dependancies.
      deps.forEach (z) ->
        shuttlesForZone.get(z).delete shuttle

      shuttleZoneDeps.delete shuttle
      
  # The shuttle is clean - but it will become dirty if these zones change.
  setCleanDeps: (shuttle, deps) ->
    # A dirty shuttle didn't move.
    dirty.delete shuttle

    deps.forEach (z) ->
      shuttlesForZone.getDef(z).add shuttle
    shuttleZoneDeps.set shuttle, deps

  forEach: (fn) -> dirty.forEach fn

ShuttleAdjacency = (shuttles, shuttleStates, shuttleGrid, currentStates) ->
  # Pairs of states in different shuttles that will be touching
  adjacentStates = new SetOfPairs

  shuttleStates.addWatch.forward (state1) ->
    return unless state1.valid
    assert state1.shuttle.used

    state1.shuttle.edges.forEach (x, y, dir) ->
      # Go through all the cells that lie just outside of the edge.
      # Note these will repeat sometimes.
      {dx, dy} = DIRS[dir]
      x += state1.dx + dx; y += state1.dy + dy

      shuttleGrid.stateGrid.get(x, y)?.forEach (state2) ->
        return if state2.shuttle is state1.shuttle
        #log 'adjacent states found', state1, state2
        assert state2.shuttle.used
        adjacentStates.add state1, state2

  shuttleStates.deleteWatch.on (state) ->
    return unless state.valid
    adjacentStates.deleteAll state
    #log 'adj', adjacentStates

  currentStates.watch.on (shuttle1, prev, state1) ->
    #log '-----> csw', state1
    assert shuttle1.used
    adjacentStates.getAll(state1)?.forEach (state2) ->
      # The shuttle might be adjacent to several other shuttles.
      #log 'checking', state1, state2
      shuttle2 = state2.shuttle
      # We need to use getImmediate here so shuttles don't collide while
      # they're moving.
      if currentStates.getImmediate(shuttle2) is state2
        # Oof. collapse.
        #log 'ccccc', shuttle1.id, shuttle2.id
        #log 'ccccc', state1, state2
        shuttles.delete shuttle1, state1 if shuttle1.used
        assert shuttle2.used
        shuttles.delete shuttle2, state2

    #log '<----- csw'

  checkEmpty: ->
    assert.equal 0, adjacentStates.size
    adjacentStates.forEach (a, b) -> throw 'Should be empty'

ShuttleOverlap = (shuttleStates, shuttleGrid, currentStates) ->
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

  willOverlap: (shuttle1, state1) ->
    # Would the named shuttle overlap with something if it enters the named state?
    overlap = no
    overlappingStates.getAll(state1)?.forEach (state2) ->
      shuttle2 = state2.shuttle
      overlap = yes if currentStates.getImmediate(shuttle2) is state2
    log 'overlap', overlap, shuttle1.id
    return overlap





module.exports = Jit = (rawGrid) ->
  grid = BaseGrid()

  # This is a watcher which emits events just before & after step().
  stepWatch = new Watcher

  engineBuffer = EngineBuffer grid
  engines = BlobFiller 'engine', engineBuffer, grid
  engineGrid = EngineGrid grid, engines

  shuttleBuffer = ShuttleBuffer()
  shuttles = BlobFiller 'shuttle', shuttleBuffer, grid

  shuttleStates = ShuttleStates grid, shuttles
  shuttleGrid = ShuttleGrid shuttleStates
  fillKeys = FillKeys grid, shuttleStates, shuttleGrid
  groups = Groups grid, engines, engineGrid, shuttleGrid, fillKeys
  stateForce = StateForce shuttleStates, groups
  groupConnections = GroupConnections groups
  regions = Regions fillKeys, groups, groupConnections
  currentStates = CurrentStates shuttles, stateForce, shuttleStates, stepWatch
  zones = Zones shuttles, regions, currentStates
  CollapseDetector grid, shuttleBuffer, shuttles, shuttleStates, shuttleGrid
  dirtyShuttles = DirtyShuttles shuttles, shuttleStates, currentStates, zones

  shuttleAdjacency = ShuttleAdjacency shuttles, shuttleStates, shuttleGrid, currentStates

  #shuttleGrid = ShuttleGrid grid, shuttles, shuttleStates, currentStates, stepWatch
  shuttleOverlap = ShuttleOverlap shuttleStates, shuttleGrid, currentStates

  modules = {grid, stepWatch, engineBuffer, engines, engineGrid, shuttleBuffer,
    shuttles, shuttleStates, shuttleGrid, fillKeys, groups, stateForce,
    groupConnections, regions, currentStates, zones, dirtyShuttles,
    shuttleAdjacency, shuttleOverlap}


  set = (x, y, v) ->
    if v in ['shuttle', 'thinshuttle']
      if !letsShuttleThrough grid.get x, y
        grid.set x, y, 'nothing'

      shuttleBuffer.set x, y, v
    else
      #assert !v? or typeof v is 'string'
      return unless !v? or typeof v is 'string'
      grid.set x, y, v
      shuttleBuffer.set x, y, null

  setGrid = (rawGrid) ->
    # Probably should delete everything here too. Eh...
    if rawGrid.base
      console.log 'Loading from new style data'
      for layer in [rawGrid.base, rawGrid.shuttles]
        for k, v of layer
          {x,y} = parseXY k
          set x, y, v
    else
      console.log 'Loading from old style data'
      for k, v of rawGrid
        {x,y} = parseXY k
        set x, y, v
    return

  setGrid rawGrid


  # ------ Step stuff ------

  calcImpulse = (f, deps) ->
    impulse = 0

    f.forEach (mult, group) ->
      log 'calculating pressure in group', group
      assert group.used
      zone = zones.getZoneForGroup group
      # Zone is null if the group is filled. But the group should never be
      # filled, because if it was this shuttle would have glommed on to
      # another shuttle anyway.
      #return if zone is null
      assert zone.used
      log 'pressure', zone.pressure
      deps.add zone

      impulse -= mult * zone.pressure
    
    return impulse

  tryMove = (shuttle, state, impulse, isTop) ->
    return unless impulse

    dir = if impulse < 0
      impulse = -impulse
      if isTop then UP else LEFT
    else
      if isTop then DOWN else RIGHT

    moved = no
    while impulse
      log 'impulse', impulse, 'dir', dir
      # We can't move any further in this direction.
      break unless (next = shuttleStates.getStateNear state, dir)

      # This shuttle is about to collide into another shuttle. ABORT!
      if shuttleOverlap.willOverlap shuttle, next
        # We'll treat a shuttle colliding with another shuttle as a successful
        # move. Remember, the shuttle won't start colliding with anything, so
        # if we hit this state either
        # - we actually did move here
        # or
        # - The other shuttle moved in our way, in which case its already moved
        # and we'll glom onto it at the end of the frame.
        #
        # This world you can see the difference:
        # ;;;;;;;
        # ; S   N
        # ;  S  ;
        # ;;;P;;;
        #
        # return state -> shuttle 2 doesn't move, and they glom together.
        # break -> both shuttles move right.
        return state

      state = next
      moved = yes
      impulse--
      
    # Return the new state.
    return if moved then state else null

  step: ->
    log '------------ STEP ------------'
    stepWatch.signal 'before'
    shuttles.flush()

    dirtyShuttles.forEach (shuttle) ->
      log 'step() looking at shuttle', shuttle

      return if shuttle.held # Manually set from the UI.

      # This is all a bit of a hack.
      #return if shuttleGrid.willCombine shuttle
       
      # Consider moving the shuttle.
      assert shuttle.used
      state = shuttle.currentState
      force = stateForce.get state
      {x:fx, y:fy} = force

      # Set of zones which, when deleted, will make the shuttle dirty again.
      # This is only used if the shuttle doesn't move. So its kinda gross that
      # we have to allocate an object here - but .. eh.
      deps = new Set

      # If we *might* move in both X and Y directions, we'll calculate them
      # both, then pick the stronger force and preferentially move in that
      # direction.
      if fx and fy
        xImpulse = calcImpulse fx, deps
        yImpulse = calcImpulse fy, deps
        if abs(yImpulse) >= abs(xImpulse)
          next = tryMove shuttle, state, yImpulse, yes
          next = tryMove shuttle, state, xImpulse, no if !next
        else
          next = tryMove shuttle, state, xImpulse, no
          next = tryMove shuttle, state, yImpulse, yes if !next
      else if (f = fx)
        xImpulse = calcImpulse f, deps
        next = tryMove shuttle, state, xImpulse, no
      else if (f = fy)
        yImpulse = calcImpulse f, deps
        next = tryMove shuttle, state, yImpulse, yes

      if next
        log '----> shuttle', shuttle.id, 'moved to', next.dx, next.dy
        currentStates.set shuttle, next
        # The shuttle is still dirty - so we're kinda done here.
      else
        # Since nothing changed, nothing should have invalidated the force.
        assert force.used

        # The shuttle didn't move.
        log '----> shuttle did not move', shuttle.id, deps
        dirtyShuttles.setCleanDeps shuttle, deps
        #log 'deps', deps

    # Tell everyone about how the current states changed. This will destroy any
    # zones for the next step() call.
    stepWatch.signal 'after'

    #@printGrid()
    @check()


  grid: grid
  modules: modules

  moveShuttle: (shuttle, state) ->
    # Try to move the named shuttle to the specified state immediately. This
    # makes a crappy fake step, and runs the edit there.
    #
    # Never call this while we're in step().
    stepWatch.signal 'before'
    if !shuttleOverlap.willOverlap shuttle, state
      currentStates.set shuttle, state
    stepWatch.signal 'after'

  check: (invasive) ->
    shuttles.check invasive
    #engines.check invasive
    engineGrid.check invasive
    shuttleGrid.check invasive
    groups.check invasive
    groupConnections.check invasive
    regions.check invasive

    # Each shuttle should be on top of nothing
    shuttles.forEach (shuttle) ->
      shuttle.eachCurrentPoint (x, y, v) ->
        baseV = grid.get x, y
        assert baseV in ['nothing', 'bridge']

    # No two shuttles should be touching to each other and un-merged
    map = new Map2
    shuttles.forEach (shuttle) ->
      shuttle.eachCurrentPoint (x, y, v) ->
        for {dx, dy} in DIRS
          s2 = map.get(x+dx, y+dy)
          assert !s2 || s2 == shuttle
        map.set x, y, shuttle


  checkEmpty: ->
    m.checkEmpty?() for k, m of modules

  printGrid: ->
    overlay = new Map2
    shuttles.forEach (s) ->
      state = s.currentState
      if !state
        return log 'no state for', s
      {dx, dy} = state
      s.points.forEach (x, y, v) ->
        overlay.set x+dx, y+dy, v

    util.printCustomGrid util.gridExtents(grid), (x, y) ->
      overlay.get(x, y) or grid.get x, y

  toJSON: ->
    # Unfortunately, the old format won't work anymore. It can't handle the
    # shuttle layer.
    json =
      base: {}
      shuttles: {}

    grid.forEach (x, y, v) ->
      assert typeof v is 'string'
      json.base["#{x},#{y}"] = v if v?

    shuttles.forEach (s) ->
      {dx, dy} = state = s.currentState
      s.points.forEach (x, y, v) ->
        json.shuttles["#{x+dx},#{y+dy}"] = v

    #console.log json
    json

  set: set


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

  jit.printGrid()
  #util.printGrid util.gridExtents(jit.grid), jit.grid

  jit.torture() if torture

  if !torture
    for [1..3]
      jit.step()
      jit.printGrid()

    log '-----'
    #jit.grid.set 5, 2, null
  
if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename


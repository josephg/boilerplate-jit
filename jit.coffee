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

letsShuttleThrough = (v) -> v in ['shuttle', 'thinshuttle', 'nothing']


Grid = (rawGrid) ->
  grid = new Map2

  forEach = (fn) ->
    grid.forEach (x, y, v) ->
      fn x, y, null, v

  # You usually want to use afterWatch to invalidate things. beforeWatch is
  # important for collapsing shuttles.
  beforeWatch = new Watcher forEach
  afterWatch = new Watcher forEach

  for k, v of rawGrid when k not in ['tw', 'th']
    {x,y} = parseXY k
    grid.set x, y, v

  beforeWatch: beforeWatch
  afterWatch: afterWatch
  get: (x, y) -> grid.get x, y
  set: (x, y, v) ->
    v = undefined if v is null
    oldV = grid.get x, y
    beforeWatch.signal x, y, oldV, v
    # oldV might have changed as a result of signaling.
    oldV = grid.get x, y

    if v != oldV
      if v
        grid.set x, y, v
      else
        grid.delete x, y
      afterWatch.signal x, y, oldV, v
      return yes
    else
      return no
  forEach: (fn) -> grid.forEach fn


GridBuffer = (match, grid) ->
  # A set of all the cells which are shuttle / thinshuttle and haven't been
  # processed.
  buffer = new Map2

  watch = new Watcher

  grid.afterWatch.forward (x, y, oldv, v) ->
    if oldv in match
      watch.signal x, y if !buffer.has x, y # Reclaim the old value

      # Then get rid of it.
      buffer.delete x, y

    if v in match
      # Reap adjacent cells. This might be special for shuttles?
      watch.signal x+dx, y+dy for {dx, dy} in DIRS
      buffer.set x, y, v

    # Technically we don't need to signal if the consumer hasn't consumed this
    # value and we deleted it. But signaling anyway is simpler and shouldn't
    # cause problems.

  watch: watch
  data: buffer
  pump: (x, y) ->
    v = buffer.get x, y
    if v
      buffer.delete x, y
    return v

# The code for finding & flood filling shuttles & engines is basically
# identical. I'll reuse it, and switch according to which type of thing I'm
# looking for.
BlobFiller = (type, grid) ->
  throw Error 'Invalid type' unless type in ['shuttle', 'engine']

  match = {
    shuttle: ['shuttle', 'thinshuttle']
    engine: ['positive', 'negative']
  }[type]

  buffer = GridBuffer match, grid

  blobs = new Set
  blobGrid = new Map2 # x,y -> shuttle.
  addWatch = new Watcher (fn) -> blobs.forEach fn
  deleteWatch = new Watcher

  buffer.watch.on (x, y) ->
    b = blobGrid.get x, y
    deleteBlob b

  deleteBlob = (b) ->
    return no unless blobs.delete b

    log "Destroyed #{type} #{b.id} at", b.points
    assert b.used
    b.used = no
    # Put the points we ate back in the buffer.
    b.points.forEach (x2, y2, v) ->
      buffer.data.set x2, y2, v

    deleteWatch.signal b
    return yes

  Blob = (x, y, v0) ->
    #log 'makeBlob at', x, y
    #v0 = buffer.peek x, y
    assert buffer.data.get(x, y) in match

    @id = makeId()
    @used = yes
    @size = 0 # For debugging
    @points = new Map2
    # Edges around the entire shuttle
    @edges = new Set3
    # Edges around v=shuttle tiles
    @pushEdges = new Set3 if type is 'shuttle'

    @numValidStates = 0 if type is 'shuttle'

    blobs.add this

    # To collect the shuttle's edge, we'll flood fill passing in a third
    # parameter - which is the direction we approached a cell from.
    util.fill3 x, y, 0, (x, y, dir, hmm) =>
      # ... So we'll hit cells up to 4 times.
      return if @points.has x, y

      v = buffer.data.get x, y

      if !v or (type is 'engine' and v != v0)
        # This adds the slot that the edge lives in.
        d = DIRS[dir]
        @edges.add x-d.dx, y-d.dy, dir
        return

      if v is 'shuttle' then for {dx, dy},d in DIRS
        v2 = grid.get x+dx, y+dy
        if v2 != 'shuttle'
          @pushEdges.add x, y, d

      buffer.pump x, y

      blobGrid.set x, y, this

      @size++
      @points.set x, y, v

      for {dx, dy},dir in DIRS
        hmm x+dx, y+dy, dir
      return

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
      new Blob x, y, v

  flushAt: (x, y) ->
    if v = buffer.data.get x, y
      new Blob x, y, v

  forEach: (fn) ->
    @flush()
    blobs.forEach fn

  get: (x, y) ->
    @flushAt x, y
    s = blobGrid.get x, y
    return s if s?.used

  delete: deleteBlob

  collapseShuttle: (shuttle, {dx, dy}) ->
    # This will happen a lot, because we'll often try to collapse multiple
    # states all in one flurry of destruction.
    return if shuttle.numValidStates is 0

    return unless deleteBlob shuttle

    log 'collapsing shuttle', shuttle.id, dx, dy

    shuttle.points.forEach (x, y, v) ->
      grid.set x, y, 'nothing'

    shuttle.points.forEach (x, y, v) ->
      grid.set x+dx, y+dy, v


  check: (invasive) ->
    @forEach(->) if invasive

    blobGrid.forEach (x, y, b) ->
      return unless b.used
      # - No two different blobs should be adjacent to each other
      for {dx, dy} in DIRS
        b2 = blobGrid.get x+dx, y+dy
        continue unless b2?.used
        assert b2 is b if type isnt 'engine' or b.type == b2.type

    blobs.forEach (b) =>
      assert b.used
      b.points.forEach (x, y, v) =>
        # - Shuttles are represented in the grid
        b2 = blobGrid.get(x, y)
        assert.equal b, b2
        assert.equal @get(x, y), b
        # - Shuttles always have all adjacent shuttle / thinshuttle points
        for {dx, dy} in DIRS when !b.points.has x+dx,y+dy
          v2 = grid.get x+dx, y+dy
          if type is 'shuttle'
            assert v2 not in ['shuttle', 'thinshuttle']
          else
            assert v2 != v


ShuttleStates = (grid, shuttles) ->
  # The set of shuttle states. A shuttle's state list starts off with just one
  # entry (the shuttle's starting state). States are added when the shuttle
  # moves
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
        deleteWatch.signal state if state.valid

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
    log 'made shuttle state for shuttle', state.shuttle.id, state.dx, state.dy if valid
    addWatch.signal state if valid
    return state

  flushStatesAt: (x, y) ->
    # Its kind of gross that I need this. Anyone listening on our watcher will
    # see states as they get created, but a shuttle might not have been created
    # yet (because of how the shuttle group works).
    #
    # NEVER CALL ME on a burning pass - or you might get wacky N^2 behaviour.
    s = shuttles.get x, y
    @get s if s

  addWatch: addWatch
  deleteWatch: deleteWatch
  get: (s) ->
    shuttleStates.get(s) or (createStateAt(s, 0, 0, []); shuttleStates.get s)

  getInitialState: (s) -> @get(s).get(0,0)

  getStateNear: (state, dir) -> # Dir is a number.
    assert state.shuttle.used
    return null unless state.valid

    {dx, dy} = DIRS[dir]
    dx += state.dx; dy += state.dy
    successor = shuttleStates.get(state.shuttle).get dx, dy

    if !successor?
      successor = createStateAt state.shuttle, dx, dy

    return successor if successor.valid


FillGrid = (shuttleStates) ->
  # Set of states which fill a given grid cell
  fillGrid = new Map2 -> new Set # Map from (x,y) -> set of states
  pointWatch = new Watcher

  # Note: It might be worth splitting this module up into two - one for the
  # fill grid and another for the keys.

  # The fill key is used for cell groups. Every adjacent grid cell with the
  # same fill key will always have the same pressure value regardless of
  # shuttle state. For now, the key is just a (stable) list of the states in
  # the fill grid.
  fillKey = new Map2 # Map from (x,y) -> string key

  # Set of the actual states which fill for a particular key
  fillStates = new Map # Map from key -> set of states
  fillStates.default = -> new Set
  # For cleaning up fillStates when a state is removed.
  # Map from state -> set of keys
  keysReferencingState = new WeakMap
  keysReferencingState.default = -> new Set

  keyWatch = new Watcher

  shuttleStates.addWatch.forward (state) ->
    #log 'shuttleStates signal', state, created
    # Populate the fill list
    state.shuttle.points.forEach (x, y, v) ->
      if v is 'shuttle' and state.valid
        #log "adding #{state.id} to #{x}, #{y}"
        x += state.dx; y += state.dy
        fillGrid.getDef(x, y).add state
        fillKey.delete x, y
        pointWatch.signal x, y

  shuttleStates.deleteWatch.on (state) ->
    state.shuttle.points.forEach (x, y, v) ->
      if v is 'shuttle' and state.valid
        x += state.dx; y += state.dy
        fillGrid.get(x, y).delete state
        fillKey.delete x, y
        pointWatch.signal x, y
    keysReferencingState.get(state)?.forEach (key) ->
      fillStates.delete key
      keyWatch.signal key

  calcKeyAt = (x, y) ->
    stateList = []
    fillGrid.getDef(x, y).forEach (state) -> stateList.push state
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

  getFilledStates: (key) -> fillStates.get key
  pointWatch: pointWatch
  keyWatch: keyWatch
  getFillKey: (x, y) ->
    shuttleStates.flushStatesAt x, y
    key = fillKey.get x, y
    if !key
      key = calcKeyAt x, y
      fillKey.set x, y, key
    return key


CellGroups = (grid, engines, fillGrid) ->
  # The first step to calculating regions is finding similar cells. Similar
  # cells are neighboring cells which will always (come hell or high water)
  # contain the same regions.
  #
  # This is very similar to shuttles, but instead of flood filling on connected
  # shuttle cells, we'll connect cells which have the same FillGrid.

  # (x, y, cell) -> cell value. This works the same as the buffers for engines
  # and shuttles.
  pendingCells = new Set3 # I really don't care what these are.

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

  grid.afterWatch.forward (x, y, oldV, v) ->
    cmax = util.cellMax oldV
    for c in [0...cmax]
      #log '---', x, y, c, oldV
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

  fillGrid.pointWatch.on (x, y) ->
    # Patch of death. Groups will stop spreading based on adjacent fill keys.
    deleteGroupsAt x, y
    edgeGrid.get(x, y)?.forEach (g) -> deleteGroup g
    #deleteGroupsAt x+dx, y+dy for {dx, dy} in DIRS

  makeGroupAt = (x, y, c) ->
    v0 = grid.get x, y

    #log 'makeGroupAt', x, y, c, v0
    assert v0?
    assert c < util.cellMax v0
    assert pendingCells.has x, y, c

    key = fillGrid.getFillKey x, y
    filledStates = fillGrid.getFilledStates key
    shuttles = util.uniqueShuttlesInStates filledStates

    group =
      _id: makeId()
      used: yes
      size: 0 # For debugging
      fillKey: key # We could just request this out of fillGrid, but its handy.
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
      if fillGrid.getFillKey(x, y) != key
        #log "wrong fill key -> '#{fillGrid.getFillKey(x, y)}'"
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

      if e = engines.get x, y
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
  shuttleStates.deleteWatch.on (state) ->
    deleteForce state

  groups.watch.on (group) ->
    #log 'got group deleted', group._id
    if (set = stateForGroup.get group)
      set.forEach (state) -> deleteForce state

      # These should be cleaned up in deleteForce when they're empty.
      assert !stateForGroup.has group

  deleteForce = (state) ->
    if (force = stateForce.get state)
      stateForce.delete state

      #log 'deleteForce', force

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

  get: (state) -> stateForce.getDef state


GroupConnections = (cellGroups) ->
  # So, here we track & report on connections between groups. Groups are either
  # complete or incomplete. Incomplete groups need to be regenerated if
  # requested, but they are useful for garbage collection.

  connections = new Map # Map from group -> set of groups it touches

  cellGroups.watch.on (group) ->
    if (gc = connections.get group)
      #log 'deleting'
      connections.delete group
      # Also any groups we've cached which connect to the deleted group will
      # need to be regenerated.
      gc.forEach (g2) ->
        set2 = connections.get g2
        if set2
          set2.complete = false
          set2.delete group

  findConnections = (group) ->
    gc = new Set
    gc.complete = true
    group.edges.forEach (x, y, c) ->
      g2 = cellGroups.get x, y, c
      #log 'g2', g2
      assert g2
      assert g2.used
      gc.add g2

      # And add a connection back. This is for garbage collection - if g2 gets
      # deleted then get(group) is called, we need to regenerate the
      # connections for group.
      set2 = connections.get g2
      if !set2
        set2 = new Set
        set2.complete = false
        connections.set g2, set2
      set2.add group

    connections.set group, gc
    gc

  get: (group) ->
    gc = connections.get group
    if !gc or !gc.complete
      gc = findConnections group
    return gc

  check: (invasive) ->
    if invasive
      cellGroups.forEach (g) =>
        set = @get g
        assert set

    connections.forEach (set, group) ->
      assert set.complete if invasive
      return unless set.complete
      log 'gggg', group
      assert group.used
      set.forEach (g2) ->
        assert g2.used

        found = no
        group.edges.forEach (x, y, c) ->
          found = yes if g2.points.has x, y, c
        assert found

  checkEmpty: ->
    assert.equal 0, connections.size


#StateEdge = (shuttleStates) ->
  # The edge of a shuttle is a set of grid cells (x, y, cell) which may

Regions = (fillGrid, cellGroups, groupConnections) ->
  # The region grid is lovely. This maps from:
  # group -> set of shuttle states -> region
  regionsForGroup = new Map
  regionsForGroup.default = (g) -> new util.ShuttleStateMap g.shuttles
    #shuttles = util.uniqueShuttlesInStates(fillGrid.getFilledStates g.fillKey)

  # This is needed for cleanup. Regions are destroyed when an adjacent group is
  # destroyed so if the group should now connect, we can take that into account.
  regionsTouchingGroup = new Map
  regionsTouchingGroup.default = -> new Set

  regions = new Set

  watch = new Watcher

  cellGroups.watch.on (group) ->
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

    util.fillGraph group0, (group, hmm) =>
      # There's three reasons we won't connect a region across group lines:
      # 1. We don't connect (in which case this won't be called)
      # 2. The other group depends on more (or less) shuttles
      if group.shuttleKey != shuttleKey
        @edges.add group
        regionsTouchingGroup.getDef(group).add this
        return

      # 3. The other group is filled in one of my states
      filledStates = fillGrid.getFilledStates group.fillKey
      filled = no
      trimmedStates.forEach (state) ->
        filled = yes if filledStates.has state
      return if filled

      # Ok, we're looking good.
      regionsForGroup.getDef(group).set trimmedStates, this
      @size++
      @groups.add group
      group.engines.forEach (e) => @engines.add e

      groupConnections.get(group).forEach hmm
      
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
    filledStates = fillGrid.getFilledStates group.fillKey
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
  # This stores & propogates the shuttle states.
  currentStates = new Map # shuttle -> state.

  # ... So, we don't want to tell everyone the current states have changed as
  # soon as we move them - other shuttles might depend on the zones where they
  # are. We'll hold any changes to the state map until step() is done then send
  # them all.
  patch = new Map

  # Its a bit of a shame we need both of these. The endStepWatch is probably
  # what you want - it is used to trigger deleting zones and regions and so on,
  # where we don't want shuttles that move early in the tick to interact with
  # shuttles which move later on.
  endStepWatch = new Watcher
  # Unfortunately, when shuttles touch they *do* have to interact.
  # immediateWatch is used to detect things like that.
  immediateWatch = new Watcher

  shuttles.addWatch.forward (s) ->
    state = shuttleStates.getInitialState s
    currentStates.set s, state
    immediateWatch.signal s, state
    endStepWatch.signal s, state

  shuttles.deleteWatch.on (s) ->
    currentStates.delete s
    # .. I'll try to clean up if you edit the grid mid-step(), but ... don't.
    patch.delete s

  stepWatch.on (time) ->
    if time is 'after'
      # Call this at the end of step()
      patch.forEach (state, shuttle) ->
        currentStates.set shuttle, state
        endStepWatch.signal shuttle, state
      patch.clear()
 
  map: currentStates
  watch: endStepWatch

  set: (shuttle, state) ->
    patch.set shuttle, state
    immediateWatch.signal shuttle, state

  immediateWatch: immediateWatch
  getImmediate: (shuttle) ->
    patch.get(shuttle) || currentStates.get(shuttle)


Zones = (regions, currentStates) ->
  # A zone is a set of regions which are connected because of the current
  # shuttle states.  These are constantly destroyed & regenerated as shuttles
  # move around.

  zones = new Set # This set may be redundant.
  zoneForRegion = new Map # region -> zone

  # For garbage collection
  zonesDependingOnShuttle = new WeakMap # shuttle -> set of zones
  zonesDependingOnShuttle.default = -> new Set

  currentStates.watch

  watch = new Watcher

  regions.watch.on (r) ->
    deleteZone zoneForRegion.get r

  currentStates.watch.on (shuttle, state) ->
    zonesDependingOnShuttle.get(shuttle)?.forEach (zone) ->
      deleteZone zone

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


DirtyShuttles = (shuttles, currentStates, zones) ->
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
  currentStates.watch.on (shuttle, state) ->
    setDirty shuttle

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


ShuttleGrid = (grid, shuttles, shuttleStates, currentStates, stepWatch) ->
  # This maps x,y -> a set of shuttles which sometimes occupy this region
  # (including in invalid states)
  # Its different from fillGrid in three ways:
  #  1. It maps to the shuttle, not the shuttle state
  #  2. It includes any shuttle which occupies a grid cell using thinshuttle
  #  3. It includes invalid states
  #
  # Its used to find & kill shuttles when the grid changes and also make sure
  # shuttles don't intersect.
  stateGrid = new Map2 -> new Set # This could probably be rewritten to use Set3.
  watch = new Watcher

  # A queue of shuttles which will be collapsed at the end of step().
  combineWatcher = new Watcher
  toCombine = new Map

  # Pairs of states that will be touching
  adjacentStates = new SetOfPairs
  # Pairs of states which will be overlapping
  overlappingStates = new SetOfPairs

  shuttleStates.addWatch.forward (state) ->
    state.shuttle.points.forEach (x, y, v) ->
      x += state.dx; y += state.dy
      stateGrid.getDef(x, y).add state
      watch.signal x, y
    addAdjacentStates state
    addOverlappingStates state

  shuttleStates.deleteWatch.on (state) ->
    #log 'deleteWatch on state', state
    state.shuttle.points.forEach (x, y, v) ->
      x += state.dx; y += state.dy
      stateGrid.get(x, y).delete state
      watch.signal x, y

    adjacentStates.deleteAll state
    overlappingStates.deleteAll state

  addAdjacentStates = (state1) ->
    state1.shuttle.edges.forEach (x, y, dir) ->
      {dx, dy} = DIRS[dir]
      x += state1.dx + dx; y += state1.dy + dy

      stateGrid.get(x, y)?.forEach (state2) ->
        return if state2.shuttle is state1.shuttle
        #log 'adjacent states found', state1, state2
        adjacentStates.add state1, state2

  addOverlappingStates = (state1) ->
    state1.shuttle.points.forEach (x, y) ->
      stateGrid.get(x+state1.dx, y+state1.dy)?.forEach (state2) ->
        return if state2.shuttle is state1.shuttle
        #log 'overlapping states', state1, state2
        overlappingStates.add state1, state2

  grid.beforeWatch.forward (x, y, oldV, v) ->
    stateGrid.get(x, y)?.forEach (state) ->
      shuttle = state.shuttle
      # We want to collapse it to its current position, not the state which
      # collided.
      state = currentStates.map.get shuttle
      shuttles.collapseShuttle shuttle, state

  currentStates.immediateWatch.on (shuttle1, state1) ->
    adjacentStates.getAll(state1)?.forEach (state2) ->
      shuttle2 = state2.shuttle
      if currentStates.getImmediate(shuttle2) is state2
        # Oof. collapse.
        toCombine.set shuttle1, state1
        toCombine.set shuttle2, state2

        #collapse shuttle1, state1
        #collapse shuttle2, state2

  stepWatch.on (time) ->
    if time is 'after' and toCombine.size
      log 'time to do some collapsin'
      toCombine.forEach (state, shuttle) -> shuttles.collapseShuttle shuttle, state
      toCombine.clear()

  watch: watch
  combineWatcher: combineWatcher
  willCombine: (shuttle) -> toCombine.has shuttle

  shuttleWillOverlap: (shuttle, state1) ->
    # Can the named shuttle enter the specified state, or would it overlap?
    overlap = no
    overlappingStates.getAll(state1)?.forEach (state2) ->
      overlap = yes if state2.shuttle != shuttle
    return overlap



module.exports = Jit = (rawGrid) ->
  grid = Grid rawGrid

  # This is a watcher which emits events just before & after step().
  stepWatch = new Watcher

  shuttles = BlobFiller 'shuttle', grid
  engines = BlobFiller 'engine', grid
  shuttleStates = ShuttleStates grid, shuttles
  fillGrid = FillGrid shuttleStates
  cellGroups = CellGroups grid, engines, fillGrid
  stateForce = StateForce shuttleStates, cellGroups
  groupConnections = GroupConnections cellGroups
  regions = Regions fillGrid, cellGroups, groupConnections
  currentStates = CurrentStates shuttles, stateForce, shuttleStates, stepWatch
  zones = Zones regions, currentStates
  dirtyShuttles = DirtyShuttles shuttles, currentStates, zones


  shuttleGrid = ShuttleGrid grid, shuttles, shuttleStates, currentStates, stepWatch

  zones: zones
  step: ->
    log '------------ STEP ------------'
    shuttles.flush()

    dirtyShuttles.forEach (shuttle) ->
      log 'step() looking at shuttle', shuttle

      # This is all a bit of a hack.
      #return if shuttleGrid.willCombine shuttle
       
      # Consider moving the shuttle.
      state = currentStates.map.get shuttle
      force = stateForce.get state

      # Set of zones which, when deleted, will make the shuttle dirty again.
      # This is only used if the shuttle doesn't move. So its kinda gross that
      # we have to allocate an object here - but .. eh.
      deps = new Set

      # Y first.
      log 'shuttle force', force
      for d in ['y', 'x'] when (f = force[d])
        impulse = 0

        f.forEach (mult, group) ->
          log 'f.forEach', group
          assert group.used
          zone = zones.getZoneForGroup group
          #return if zone is null # Zone is null if the group is filled.
          assert zone.used
          log 'pressure', zone.pressure
          deps.add zone

          impulse -= mult * zone.pressure
        
        log 'd', d, 'impulse', impulse
        continue unless impulse

        dir = if impulse < 0
          impulse = -impulse
          if d is 'y' then UP else LEFT
        else
          if d is 'y' then DOWN else RIGHT

        moved = no
        while impulse
          log 'impulse', impulse, 'dir', dir
          # We can't move any further in this direction.
          break unless (next = shuttleStates.getStateNear state, dir)

          # This shuttle is about to collide into another shuttle. ABORT!
          break if shuttleGrid.shuttleWillOverlap shuttle, next
          state = next
          moved = yes
          impulse--
          
        if moved
          log 'shuttle', shuttle.id, 'moved to', state.dx, state.dy
          currentStates.set shuttle, state
          # The shuttle is still dirty - so we're kinda done here.
          return

      # The shuttle didn't move.
      dirtyShuttles.setCleanDeps shuttle, deps
      log '----> shuttle did not move', shuttle, deps
      #log 'deps', deps
            
    # Tell everyone about how the current states changed. This will destroy any
    # zones for the next step() call.
    stepWatch.signal 'after'

  check: (invasive) ->
    shuttles.check invasive
    #engines.check invasive
    cellGroups.check invasive
    groupConnections.check invasive
    regions.check invasive

  checkEmpty: ->
    cellGroups.checkEmpty()
    groupConnections.checkEmpty()
    regions.checkEmpty()
    zones.checkEmpty()

  printGrid: ->
    overlay = new Map2
    shuttles.forEach (s) ->
      {dx, dy} = state = currentStates.map.get s
      s.points.forEach (x, y, v) ->
        overlay.set x+dx, y+dy, v

    util.printCustomGrid util.gridExtents(grid), (x, y) ->
      if v = overlay.get x, y
        return v
      else
        v = grid.get x, y
        if v in ['shuttle', 'thinshuttle'] then 'nothing' else v

  toJSON: ->
    json = {}
    grid.forEach (x, y, v) ->
      v = 'nothing' if v in ['shuttle', 'thinshuttle']
      json["#{x},#{y}"] = v if v?
    shuttles.forEach (s) ->
      {dx, dy} = state = currentStates.map.get s
      s.points.forEach (x, y, v) ->
        json["#{x+dx},#{y+dy}"] = v
    json

  grid: grid
  groups: cellGroups
  shuttles: shuttles
  engines: engines
  getShuttlePos: (shuttle) ->
    state = currentStates.map.get shuttle
    return {dx:state.dx, dy:state.dy}



###
start = Date.now()
jit = new Jit require './cpu.json'
#jit.torture()
end = Date.now()
console.log end - start

###
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
    for [1..1]
      jit.step()
      jit.printGrid()

    log '-----'
    #jit.grid.set 5, 2, null
  
if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename


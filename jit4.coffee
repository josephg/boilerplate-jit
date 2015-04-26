# This jit is designed as an ecosystem of signaling parts.
Watcher = require './watch2'
{Map2, Map3, Set2, Set3} = require './collections2'
{parseXY, fill, fillCells, DIRS} = util = require './util2'
log = require './log'
assert = require 'assert'
mersenne = require 'mersenne'

makeId = do ->
  nextId = 1
  -> nextId++

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

Shuttles = (grid) ->
  buffer = GridBuffer ['shuttle', 'thinshuttle'], grid

  shuttles = new Set
  shuttleGrid = new Map2 # x,y -> shuttle.
  watch = new Watcher

  buffer.watch.on (x, y) ->
    deleteShuttleAt x, y

  deleteShuttleAt = (x, y) ->
    s = shuttleGrid.get x, y
    return no unless shuttles.delete s

    log "Destroyed shuttle #{s.id} at", x, y
    assert s.used
    s.used = no
    # Put the points we ate back in the buffer.
    s.points.forEach (x2, y2, v) ->
      buffer.data.set x2, y2, v

    watch.signal s
    return yes

  makeShuttle = (x, y) ->
    #log 'makeShuttle at', x, y
    #v = buffer.peek x, y
    assert buffer.data.get(x, y) in ['shuttle', 'thinshuttle']

    s =
      id: makeId()
      used: yes
      size: 0 # For debugging
      numValidStates: 0
      points: new Map2

    shuttles.add s

    fill x, y, (x, y) =>
      v = buffer.pump x, y
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
    buffer.data.forEach (x, y, v) ->
      makeShuttle x, y

    shuttles.forEach fn

  get: (x, y) ->
    if buffer.data.get x, y
      makeShuttle x, y

    s = shuttleGrid.get x, y
    return s if s?.used

  collapse: (s) ->
    # When any cells are edited along a shuttle's path, we should move the
    # shuttle into its current position (its current state) in the grid. This
    # will force the shuttle to be regenerated (along with its states, starting
    # with the current state).
    throw Error 'not implemented'

  check: (invasive) ->
    @forEach(->) if invasive

    shuttleGrid.forEach (x, y, s) ->
      return unless s.used
      # - No two different shuttles should be adjacent to each other
      for {dx, dy} in DIRS
        s2 = shuttleGrid.get x+dx, y+dy
        assert s2 is s if s2?.used

    shuttles.forEach (s) =>
      assert s.used
      s.points.forEach (x, y, p) =>
        # - Shuttles are represented in the grid
        s2 = shuttleGrid.get(x, y)
        assert.equal s, s2 if s2?.used
        assert.equal @get(x, y), s
        # - Shuttles always have all adjacent shuttle / thinshuttle points
        for {dx, dy} in DIRS when !s.points.has x+dx,y+dy
          v2 = grid.get x+dx, y+dy
          assert v2 not in ['shuttle', 'thinshuttle']


ShuttleStates = (grid, shuttles) ->
  # The set of shuttle states. A shuttle's state list starts off with just one
  # entry (the shuttle's starting state). States are added when the shuttle
  # moves
  shuttleStates = new Map # shuttle -> Map2(dx, dy) -> states

  watch = new Watcher (fn) ->
    shuttleStates.forEach (shuttle, states) ->
      states.forEach (x, y, state) ->
        fn state, yes

  shuttles.watch.on (shuttle) ->
    # Shuttle delete messages
    states = shuttleStates.get shuttle
    if states
      shuttleStates.delete shuttle
      states.forEach (x, y, state) ->
        watch.signal state, no

  canShuttleFitAt = (shuttle, dx, dy) ->
    shuttle.points.forEach (x, y) =>
      # The grid cell is un-enterable.
      if !letsShuttleThrough grid.get(x+dx, y+dy)
        return no
    return yes

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

    log 'created new state', state
    watch.signal state, yes
    return state

  flushStatesAt: (x, y) ->
    # Its kind of gross that I need this. Anyone listening on our watcher will
    # see states as they get created, but a shuttle might not have been created
    # yet (because of how the shuttle group works).
    #
    # NEVER CALL ME on a burning pass - or you might get wacky N^2 behaviour.
    s = shuttles.get x, y
    @get s if s

  watch: watch
  get: (s) ->
    shuttleStates.get(s) or (createStateAt(s, 0, 0, []); shuttleStates.get s)

  getInitialState: (s) -> @get(s).get(0,0)

  getStateNear: (state, dir) -> # Dir is a number.
    assert state.shuttle.used
    return null unless state.valid

    {dx, dy} = DIRS[dir]
    dx += state.dx; dy += state.dy
    successor = shuttleStates.get(s).get dx, dy

    if successor is null
      successor = createStateAt state.shuttle, dx, dy

    return successor


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

  shuttleStates.watch.forward (state, created) ->
    #log 'shuttleStates signal', state, created
    if created
      # Populate the fill list
      state.shuttle.points.forEach (x, y, v) ->
        if v is 'shuttle' and state.valid
          x += state.dx; y += state.dy
          fillGrid.get(x, y).add state
          fillKey.delete x, y
          pointWatch.signal x, y
    else
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
    fillGrid.get(x, y).forEach (state) -> stateList.push state
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

CellGroups = (grid, fillGrid) ->
  # The first step to calculating regions is finding similar cells. Similar
  # cells are neighboring cells which will always (come hell or high water)
  # contain the same regions.
  #
  # This is very similar to shuttles, but instead of flood filling on connected
  # shuttle cells, we'll connect cells which have the same FillGrid.

  # (x, y, cell) -> cell value
  pendingCells = new Set3 # I really don't care what these are.

  # (x, y, cell) -> group
  groupGrid = new Map3
  groups = new Set

  watch = new Watcher

  deleteGroupAt = (x, y, c) ->
    group = groupGrid.get x, y, c
    return unless group
    log 'deleting group', group

    # Invalidate g!
    group.used = no # More for debugging than anything.
    groups.delete group
    watch.signal group

    # Recycle the points
    group.points.forEach (px, py, pc, pv) ->
      pendingCells.add px, py, pc
      groupGrid.delete px, py, pc

  grid.watch.forward (x, y, oldV, v) ->
    cmax = util.cellMax oldV
    for c in [0...cmax]
      deleteGroupAt x, y, c
      pendingCells.delete x, y, c

    for {dx, dy} in DIRS
      cmax = util.cellMax grid.get x+dx, y+dy
      for c in [0...cmax]
        deleteGroupAt x+dx, y+dy, c

    cmax = util.cellMax v
    for c in [0...cmax]
      pendingCells.add x, y, c

  fillGrid.pointWatch.on (x, y) ->
    v = grid.get x, y
    cmax = util.cellMax v
    deleteGroupAt x, y, c for c in [0...cmax]

  makeGroupAt = (x, y, c) ->
    assert pendingCells.has x, y, c
    key = fillGrid.getFillKey x, y
    filledStates = fillGrid.getFilledStates key
    shuttles = util.uniqueShuttlesInStates filledStates

    group =
      used: yes
      size: 0 # For debugging
      fillKey: key # We could just request this out of fillGrid, but its handy.
      points: new Map3
      edges: new Set3 # x, y, c
      shuttles: shuttles
      shuttleKey: shuttles.map((s) -> "#{s.id}").join ' '

    #log 'makeGroupAt', x, y, c, key

    fillCells grid, x, y, c, (x, y, c, v) ->
      #log 'fillCells', x, y, c, v
      if fillGrid.getFillKey(x, y) == key
        group.points.set x, y, c, v
        group.size++
        assert !groupGrid.has x, y, c
        groupGrid.set x, y, c, group
        assert pendingCells.has x, y, c
        pendingCells.delete x, y, c
        return yes
      else
        group.edges.add x, y, c
        return no


    groups.add group
    #log 'made group', group
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

    return g

  forEach: (fn) ->
    # This is an interesting function - more useful for debugging than anything
    # else. By fetching all groups, you'll force them all to get generated. Not
    # something you ever really want to do - but eh.
    pendingCells.forEach (x, y, c) ->
      makeGroupAt x, y, c

    groups.forEach fn


GroupConnections = (cellGroups) ->
  # So, here we track & report on connections between groups. Groups are either
  # complete or incomplete. Incomplete groups need to be regenerated if
  # requested, but they are useful for garbage collection.

  connections = new Map # Map from group -> set of groups it touches

  cellGroups.watch.on (group) ->
    if (gc = connections.get group)
      log 'deleting'
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
      assert group.used
      set.forEach (g2) ->
        assert g2.used

        found = no
        group.edges.forEach (x, y, c) ->
          found = yes if g2.points.has x, y, c
        assert found


StateGrid = (shuttleStates) ->
  # This maps x,y -> a set of shuttles which sometimes occupy this region
  # (including in invalid states)
  # Its different from fillGrid in three ways:
  #  1. It maps to the shuttle, not the shuttle state
  #  2. It includes any shuttle which occupies a grid cell using thinshuttle
  #  3. It includes invalid states
  #
  # Its used to find & kill shuttles when the grid changes.
  stateGrid = new Map2 -> new Set
  watch = new Watcher

  shuttleStates.watch.forward (state, created) ->
    if created
      state.shuttle.points.forEach (x, y, v) ->
        x += state.dx; y += state.dy
        stateGrid.get(x, y).add state.shuttle
        watch.signal x, y
    else
      state.shuttle.points.forEach (x, y, v) ->
        x += state.dx; y += state.dy
        stateGrid.get(x, y).delete state.shuttle
        watch.signal x, y

#StateEdge = (shuttleStates) ->
  # The edge of a shuttle is a set of grid cells (x, y, cell) which may

Regions = (fillGrid, cellGroups, groupConnections) ->
  # The region grid is lovely. This maps from:
  # group -> set of shuttle states -> region
  regionsForGroup = new Map
  regionsForGroup.default = (g) ->
    shuttles = util.uniqueShuttlesInStates(fillGrid.getFilledStates g.fillKey)
    new util.ShuttleStateMap shuttles

  regions = new Set

  watch = new Watcher

  cellGroups.watch.on (group) ->
    map = regionsForGroup.get group
    return unless map
    regionsForGroup.delete group

    map.forEachValue (region) ->
      deleteRegion region
      watch.signal region

  deleteRegion = (region) ->
    region.used = no
    region.groups.forEach (group) ->
      regionsForGroup.get(group)?.delete region.states

  createRegion = (group0, currentStates) ->
    log 'createRegion', group0, currentStates
    assert regionsForGroup.getDef(group0).isDefinedFor currentStates

    shuttleKey = group0.shuttleKey

    # A map of just the states that are referenced by the group
    trimmedStates = new Set
    invalid = no
    group0.shuttles.forEach (s) ->
      state = currentStates.get s
      trimmedStates.add state
      invalid = yes if fillGrid.getFilledStates(group0.fillKey).has state

    # We were asked to create a region for a group with shuttle states that
    # fill the group.
    if invalid
      # Null = this group is filled here; no region is possible.
      regionsForGroup.getDef(group0).set currentStates, null
      log '-> invalid'
      return null

    region =
      used: yes
      size: 0
      groups: new Set
      states: trimmedStates

    util.fillGroups group0, (group) ->
      # There's three reasons we won't connect a region across group lines:
      # 1. We don't connect (in which case this won't be called)
      # 2. The other group depends on more (or less) shuttles
      return null if group.shuttleKey != shuttleKey

      # 3. The other group is filled in one of my states
      filledStates = fillGrid.getFilledStates group.fillKey
      filled = no
      trimmedStates.forEach (state) ->
        filled = yes if filledStates.has state
      return null if filled

      # Ok, we're looking good.
      regionsForGroup.getDef(group).set trimmedStates, region
      region.size++
      region.groups.add group

      return groupConnections.get group
      
    assert region.size
    regions.add region
    region

  get: (group, currentStates) ->
    map = regionsForGroup.getDef group
    region = map.get currentStates
    return null if region is null # The group is filled right now.

    if !region
      region = createRegion group, currentStates

    return region



Jit = (rawGrid) ->
  grid = Grid rawGrid
  #engineBuffer = GridBuffer ['positive', 'negative'], grid

  shuttles = Shuttles grid
  shuttleStates = ShuttleStates grid, shuttles
  fillGrid = FillGrid shuttleStates
  stateGrid = StateGrid shuttleStates
  cellGroups = CellGroups grid, fillGrid
  groupConnections = GroupConnections cellGroups
  regions = Regions fillGrid, cellGroups, groupConnections

  #shuttles.forEach (s) ->
  #  log 'shuttle', s
  #grid.set 2,0, 'shuttle'
  #shuttles.forEach (s) ->
  #  log 'shuttle', s

  #grid.set 3, 0, null


  cellGroups.forEach (group) ->
    #log 'group', group
    #log 'connections', groupConnections.get group
  log '----'
  #grid.set 6, 2, 'nothing'
  log '----'

  currentState = new Map
  shuttles.forEach (s) ->
    currentState.set s, shuttleStates.getInitialState(s)

  cellGroups.forEach (group) ->
    #log 'region', regions.get group, currentState

  ###


  currentState = new Map
  dangerousShuttles = new Set # Shuttles which might move next tick
  shuttles.forEach (s) ->
    currentState.set s, shuttleStates.getInitialState(s)
    dangerousShuttles.add s
  
  grid.forEach (x, y, v) ->
    log 'fillKey', x, y, fillGrid.getFillKey x, y

  grid.set 3, 0, null
  grid.forEach (x, y, v) ->
    log 'fillKey', x, y, fillGrid.getFillKey x, y

  step: ->
    dangerousShuttles.forEach (s) ->
      # 1. Calculate up/down force
      #   - Get all neighboring zones given current state
      #
      #
      # 2. Try and move up/down if needed
      #
      # 3. Calculate left/right force
      # 4. Try and move left/right, ...

  ###

  torture: ->
    randomValue = randomWeighted [
      null, 2
      'nothing', 10
      'positive', 1
      'negative', 1
      'shuttle', 1
      'thinshuttle', 1
    ]


    for iter in [1...1000]
      log "----- iter #{iter}"
      for [1...10]
        x = mersenne.rand() % 4
        y = mersenne.rand() % 4
        v = randomValue()
        log 'set', x, y, v
        grid.set x, y, v

      #@debugPrint()

      try
        invasive = iter % 2 == 0
        shuttles.check invasive
        groupConnections.check invasive

        currentState = new Map
        shuttles.forEach (s) ->
          currentState.set s, shuttleStates.getInitialState(s)

        cellGroups.forEach (group) ->
          r = regions.get group, currentState
          assert r is null || r.used

      catch e
        log '****** CRASH ******'
        #@debugPrint()
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



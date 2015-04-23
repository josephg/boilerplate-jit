# boilerplate jit parser. This is an optimized version of the boilerplate
# compiler, but without any codegen. It uses a lot of newer ideas to fix
# missteps with the boilerplate compiler. The idea is that when the world is
# first scanned, we don't assume any shuttles can move. When shuttles actually
# move to new places, we re-flood fill the minimal amount of stuff and expand
# the model accordingly.
#
# Say, given the world:
#
#     S
# N--------S
#
# First, we flood fill out the one region. The region is non-conditional, and connects:
#
# r1 {engines:e1} force on {shuttle 1 down}, {shuttle 2 left} connections {}
# dirty regions: r1
#
# calculate transient regions:
# tr1 = {r1, force:-1}
#
# -> unstable shuttles: s1, s2
#
# Shuttles move - scan s1, s2. They both move to:
#
#     -
# N---S---S-
#
# We add state 2 to each shuttle.
# - The shuttles invading region r1 means we discard r1 (entered region) and
#   recalculate it from the edges of the entered regions. (Although we should
#   do all the discarding first, then all the recalculating).
# - Each space vacated by the shuttles should also be calculated (if its not there already).
#
# In both cases, we might need to iterate through every state of the shuttle
# and calculate connectivity individually in each one. Although there's surely
# ways to speed this up & reuse stuff.
#
# So:
#     3
# N222455567
#
# regions 2 and 5 are normal regions. Region 3 is unnecessary. Region 4 is dependant on shuttle S1's state:
# {0:{connects 2 and 5, pulls down on S1}, 1:{does nothing}}
#
# .... no, this isn't good enough - we also need to calculate the negative pull of 2 on S1 in state 1!
#
#
#
#
#
# then in step() we do the following steps:
#
#
#
# transient regions recalculated
# resultant force calculated
# shuttles move
# (if new state, regions invalidated)
# (regions recalculated)
# transient regions invalidated

{parseXY, fill} = util = require './util'
Switch = require './switch'

log = (args...) ->
  if require
    {inspect} = require 'util'
    f = (a) -> if typeof a is 'string' then a else inspect a, {depth:5, colors:true}
    console.log args.map(f).join ' '
  else
    console.log args...

UP=0; RIGHT=1; DOWN=2; LEFT=3
DN =
  0: 'UP'
  1: 'RIGHT'
  2: 'DOWN'
  3: 'LEFT'

dirs = [
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

cellAt = (grid, x, y, dir) ->
  v = grid["#{x},#{y}"]
  #log 'cellat', x, y, DN[dir], v
  switch v
    # Shuttle in this list because there's no guarantee that the shuttle is
    # still here later. Be careful!
    when 'nothing', 'thinsolid', 'thinshuttle', 'shuttle'
      [x, y]
    when 'bridge'
      [x, y, if dir in [UP, DOWN] then 0 else 1]
    when 'negative', 'positive'
      [x, y, dir]
    else
      null

cellOpposite = (grid, x, y, dir) ->
  {dx, dy} = dirs[dir]
  #log 'cellOpposite', x, y, DN[dir], x+dx, y+dy
  cellAt grid, x+dx, y+dy, oppositeDir(dir)

# We don't need isTop for this (its redundant with dir) but you should still
# think of (x,y) as (x,y,isTop).
# (1,1,true,DOWN) would be (1,1).
# (1,1,true,UP) would be (1,0).
cellOppositeEdge = (grid, x, y, dir) ->
  --x if dir is LEFT
  --y if dir is UP
  cellAt grid, x, y, oppositeDir(dir)

fillRegion = (grid, initialCell, expandFn) ->
  toExplore = []
  visited = {}

  hmm = (slot) =>
    return unless slot
    sk = slot.join ','
    if !visited[sk]
      #console.log 'expanding', r, 'to', x, y, isTop
      visited[sk] = true
      toExplore.push slot

  hmm initialCell
  
  while slot = toExplore.pop() when expandFn slot, hmm
    [x, y, n] = slot

    # We need to check for connectivity via all adjoining cells
    v = grid["#{x},#{y}"]
    switch v
      when 'bridge'
        if n is 0
          hmm cellOpposite grid, x, y, UP
          hmm cellOpposite grid, x, y, DOWN
        else
          hmm cellOpposite grid, x, y, LEFT
          hmm cellOpposite grid, x, y, RIGHT
      when 'nothing', 'thinsolid', 'thinshuttle', 'shuttle'
        hmm cellOpposite grid, x, y, d for d in [0...4]
      when 'positive', 'negative'
        hmm cellOpposite grid, x, y, n

# Are the shuttles in list A a proper subset of the shuttles in list B? Skips
# by 2, because reasons.
isListSubsetOf = (a, b) ->
  return true if !a
  return false if !b

  ai = bi = 0

  # We're in a sense looking for a shuttle in list A thats not in list B - then we return false.
  while ai < a.length
    while bi < b.length and b[bi] < a[ai]
      bi += 2

    return false if bi > b.length or b[bi] != a[ai]

    ai += 2
    bi += 2

  return true

sidsInGridFill = (list) ->
  return [] unless list
  sid for sid in list by 2

# Iterate through all states in the sidList, calling fn on each. You can pass
# shuttleStates in to initialize the process - its an optional argument.
permuteStates = (shuttles, sidList, shuttleStates, fn) ->
  if typeof shuttleStates is 'function'
    fn = shuttleStates
    shuttleStates = {}

  totalNumber = 1
  for sid in sidList
    totalNumber *= shuttles[sid].states.length

  for i in [0...totalNumber]
    v = i
    for sid,k in sidList
      num = shuttles[sid].states.length
      shuttleStates[sid] = v % num
      v = (v / num)|0

    #log 'state set', shuttleStates
    fn shuttleStates

  return



# This will happen for all tiles which aren't engines and aren't in shuttle zones
# (so, engines, empty space, grills and bridges)
letsAirThrough =
  nothing: yes
  thinsolid: yes
  bridge: yes
  thinshuttle: yes


###
lat, engine: yes
lat, shuttle: yes
engine, shuttle: yes
x, wall: no
shuttle, shuttle: no
###

oppositeDir = (dir) -> (dir+2) % 4

pressureOf = (v) -> if v is 'positive' then 1 else -1

abs = (x) -> if x < 0 then -x else x



class Jit
  # ******** helpers

  get: (x,y) -> @grid["#{x},#{y}"]
  #regionAt: (slot) -> @regionInSlot[slot.join ',']

  id: -> @nextId++
  
  cellAt: (x, y, dir) -> cellAt @grid, x, y, dir
  cellOpposite: (x, y, dir) -> cellOpposite @grid, x, y, dir
  cellOppositeEdge: (x, y, dir) -> cellOppositeEdge @grid, x, y, dir

  # ********
 

  constructor: (@grid, @opts = {}) ->
    log @grid
    @fillMode = @opts.fillMode || 'engines'
    #@fillMode = 'shuttles'

    throw Error "Invalid fillMode #{@fillMode}" unless @fillMode in ['engines', 'shuttles', 'all']

    #console.log @grid
    @extents = util.gridExtents @grid
    util.printGrid @extents, @grid

    @nextId = 1

    # This engines and shuttles in the world. These are constant - changing
    # them (for now) requires recalculating everything.
    @engines = []
    @shuttles = []

    # The regions are deleted and recreated whenever a shuttle nearby moves in
    # to an unexpected place. Each region has a list of shuttle state
    # dependancies (preconditions) and an adjacency list.
    @regions = {}

    # Zones are recreated when there's any change to
    # the connectivity graph.
    @zones = {}


    # This maps x,y of cells to a list of shuttle/state pairs which we know can
    # fill the grid cell. Always sorted by shuttle id then state id.
    # eg: [1,0, 1,2, 1,3]
    @gridFill = {}

    # This is a map from x,y to a string key of which shuttles the cell is
    # filled by sometimes. Eg "1,2,4"
    @gridKey = {}


    # map from grid position -> engine
    @engineGrid = {}

    # map from grid position -> list of shuttles which can occupy that
    # position. This includes thinshuttle.
    # [s1, s2, s3]
    @shuttleGrid = {}
    

    # map from "x,y[,slot]" to region or region list. Slots are directions for
    # engines, [0,1] for bridges, unused for thinsolid / thinshuttle.
    #
    # The value is either null (always filled), a region (no shuttles) or a
    # list. If its a list, this maps from the cross product of all shuttles
    # which fill the grid cell to the correponding region.
    @switchInSlot = {}



    # map from x,y -> region which filled through that point.
    #@regionGrid = {}

    # This maps from "x,y,dir" -> pair of regions abutting the edge.
    # There is an edge between regions whenever they touch.
    #@edges = {}


    @killRegionInSlot = []
    #@genRegionInSlot = []

    @genZoneForRegion = []


    # set of edges which need a region filled out.
    #@dirtyEdges = {}

    # set of regions which need a derived region
    #@dirtyRegions = {}


    @initialize()

  initialize: ->
    # Scan for all engines, shuttles.
    @annotateGrid()

    @shuttleState = new Int32Array @shuttles.length

    #log 'grs', @genRegionInSlot

    #@fillRegions()

    #for s in @shuttles
    #  @makeStateForces s, state for state in s.states
    

    #@makeRegion slot, states while {slot, states} = @genRegionInSlot.pop()

    log 'engines', @engines
    log 'shuttles', @shuttles
    log @regions, @regionGrid
    ###

    log @regionInSlotByKey
    ###

  annotateGrid: ->
    for k,v of @grid when k not in ['tw', 'th']
      {x,y} = parseXY k
      switch v
        when 'positive', 'negative'
          # Mark these - we'll need them later.
          pressure = pressureOf v
          id = @engines.length
          engine = {x, y, pressure, regions:[], id}
          @engines.push @engineGrid["#{x},#{y}"] = engine

          #if @fillMode in ['engines', 'all']
          #  for d in [0...4]
          #    @genRegionInSlot.push {slot:[x,y,d], states:{}}

        when 'shuttle', 'thinshuttle'
          # flood fill the shuttle extents.
          continue if @shuttleGrid["#{x},#{y}"]?

          id = @shuttles.length
          @shuttles.push s =
            id: id
            points: {} # Grid of points in the shuttle in the base state
            states: [] # List of the {dx,dy,pushedBy} of each state
            edge: [] # list of {x,y,isTop,dir} edges that we care about.
            currentid: 0
            exclusiveWith: [] # shuttle,state pairs which can inhabit the same cell

            #fill: {} # Map from x,y -> [true if filled in state=index]
            #adjacentTo: {} # Map from {x,y} -> [region id]
            #moves: {x:false, y:false}
            #immobile: v is 'thinshuttle' # Immobile if only thinshuttle, no states or no pressure possible.
            #pushedBy: [] # List of {rid, mx, my} across all states

          # Flood fill the shuttle
          fill {x,y}, (x, y) =>
            v = @get x, y
            #addDirtyEdgesBounding x, y, 'negative' if @fillMode in ['all', 'engines']
            if v in ['shuttle', 'thinshuttle']
              #s.immobile = false if s.immobile && @get(x,y) is 'shuttle'

              @shuttleGrid["#{x},#{y}"] = [s]
              s.points["#{x},#{y}"] = v

              # Find all the cells that will pull/push us.
              if v is 'shuttle' then for {ex,ey,isTop},dir in edges
                {dx,dy} = dirs[dir]
                v2 = @get x+dx, y+dy
                continue if v2 is 'shuttle' # ignore internal borders
                s.edge.push {x:x+ex, y:y+ey, isTop, dir}

                #if @fillMode in ['shuttles', 'all']
                #  cell = @cellOpposite x, y, dir
                #  @genRegionInSlot.push {slot:cell, states:{}} if cell isnt null

              true
            else
              false

          @addShuttleState s, 0, 0




  ########## SHUTTLES


  shuttleFillsCell: (x, y, s, stateid) ->
    #log 'shuttleFillsCell', x, y, s.id, stateid
    k = "#{x},#{y}"
    if !@gridFill[k]
      @gridFill[k] = [s.id, stateid]
      @gridKey[k] = "#{s.id}"
    else
      list = @gridFill[k]
      sids = {}
      for sid, i in list by 2
        if sid is s.id and list[i+1] is stateid
          # We already know that the shuttle fills the cell sometimes. No need to
          # get huffy about it.
          return
        sids[sid] = true

      list.push s.id
      list.push stateid
      # This is going to do a lexographic sort, but it doesn't really matter so
      # long as its stable.
      @gridKey[k] = Object.keys(sids).sort().join ','


    # Lets invalidate some regions!
    if (sw = @switchInSlot[k])
      sw.each (r) ->
        r.obsolete = yes

        # TODO: Remove r's region connections

        for {state} in r.appliesPressureTo
          delete state.force

      delete @switchInSlot[k]

      ###
      for {dx,dy}, dir in edges
        list2 = @gridFill["#{x+dx},#{y+dy}"]
        if list2 then for sid,i in list2 by 2
          stateid = list2[i+1]
          # Force the shuttle to rethink its forces
      ###
      
    sids = sidsInGridFill @gridFill[k]
    permuteStates @shuttles, sids, (shuttleStates) ->
      # Teeechnically this check is not necessary - we check again in makeRegion.
      if shuttleStates[s.id] != stateid
        @genRegionInSlot.push {slot:[x,y], states:shuttleStates}

    

    #if r = @regionInSlotByKey[k]
    #  @killRegionInSlot.push {r, x, y}

  addShuttleState: (s, dx, dy) ->
    # successor is the connected state in each direction
    stateid = s.states.length
    state = {dx, dy, flags:0, successor: [-1,-1,-1,-1], id:stateid}
    s.states.push state

    for k,v of s.points when v is 'shuttle'
      {x,y} = parseXY k
      @shuttleFillsCell x+dx, y+dy, s, stateid

    # Find which directions the shuttle can move in from state 0 (here)
    @calcShuttleDirs s, state

    #console.log 'state', state
    stateid

  eachStateEdge: (s, state, fn) ->
    {dx, dy, flags} = state
    for {x,y,dir} in s.edge # Edge also has isTop, but its redundant.
      slot = @cellOppositeEdge x+dx, y+dy, dir
      continue unless slot
      sw = @makeSwitchInSlot slot
      # If sw is missing, we should be up against a wall or something. No
      # biggie.
      continue unless sw
      fn slot, sw, dir

  makeStateForces: (s, state) ->
    return state.force if state.force

    log 'makeStateForces-----------'
    # For this state, first we'll figure out the surrounding regions & figure
    # out which other shuttle states we depend on. (Based on any potentially
    # adjacent shuttles). Usually this will be *none*.

    {dx, dy, flags} = state

    sids = {}
    shuttleStates = {}
    shuttleStates[s.id] = state.id

    @eachStateEdge s, state, (slot, sw) ->
      for sid in sw.sidList when sid != s.id
        sids[sid] = true

    sw = new Switch (Object.keys(sids).map((x) -> +x).sort())

    permuteStates @shuttles, sids, shuttleStates, =>
      log 'permute', shuttleStates
      xForce = {}
      yForce = {}
      @eachStateEdge s, state, (slot, sw, dir) =>
        #log x, y, DN[dir]
        log "switch at #{slot} applies #{DN[oppositeDir(dir)]} pressure to #{s.id} #{state}"
        r = @makeRegion slot, shuttleStates

        r.appliesPressureTo.push {s,state}

        if dir in [UP, DOWN]
          yForce[r.id] ?= 0
          yForce[r.id] += dirs[dir].dy
        else
          xForce[r.id] ?= 0
          xForce[r.id] += dirs[dir].dx

      # This could probably be munged into a slightly more useful form, but eh.
      sw.set shuttleStates, {x:xForce, y:yForce}
      log 'set', shuttleStates, xForce, yForce

    state.force = sw


  calcShuttleDirs: (s, state) ->
    flags = 0
    for {dx, dy},i in dirs
      flags |= (1<<i) if @canShuttleMoveTo s, dx + state.dx, dy + state.dy

    state.flags = flags

  canShuttleMoveTo: (s, dx, dy) ->
    # Can the specified shuttle be in the specified dx/dy positions?
    for k of s.points
      {x, y} = parseXY k
      x += dx; y += dy
      continue if @get(x, y) in ['nothing', 'shuttle', 'thinshuttle']
      return no
    return yes








  ####### REGIONS

  makeSwitchInSlot: (slot) ->
    [x0, y0, n0] = slot
    k0 = "#{x0},#{y0}"
    key = @gridKey[k0]

    # Early exit if the switch already exists & is current.
    sw = @switchInSlot[slot]
    return sw if sw and sw.key is (key || '') and !sw.obsolete

    # Don't create stupid switches. (This might be a pointless optimization...)
    v0 = @get x0, y0
    return unless v0
    if v0 in ['positive', 'negative']
      {dx, dy} = dirs[n0]
      return null if !@get(x0+dx, y0+dy)

    # Now we need to go through all permutations of the shuttles in question
    # and figure out what the switch switches.
    return @switchInSlot[slot] = new Switch sidsInGridFill @gridFill

  makeRegion: (slot0, shuttleStates) ->
    log 'mr', slot0, shuttleStates
    console.trace 'blerp' unless slot0
    [x0, y0, n0] = slot0
    k0 = "#{x0},#{y0}"
    key0 = @gridKey[k0]
    list0 = @gridFill[k0]

    # No region in invalid slots
    sw = @makeSwitchInSlot slot0
    return null unless sw
    # No region if one already exists
    r = sw.get shuttleStates
    #console.log 'already region', slot0, sw, r
    return r if r and !r.obsolete
    # No region if the seed slot is filled in the current shuttleStates.
    if list0 then for sid, i in list0 by 2
      state = list0[i+1]
      return null if shuttleStates[sid] == state

    id = "r#{@id()}"
 
    log "creating region #{id} from [#{slot0.join ','}] with key #{key0}" #, shuttleStates
    @regions[id] = r =
      engines: []
      #connections: null
      size: 0 # Just for debugging.
      appliesPressureTo: []
      id: id
      key: key0
      obsolete: no
      zone: null

    @genZoneForRegion.push r

    # Up connections are connections to switches which have shuttle constraints
    # we don't have.
    upConnections = {}
    # Down connections are connections to regions which have a strict subset of
    # the constraints we have.
    downConnections = {}

    # We'll hit the same engine multiple times. Using a map to dedup, then
    # we'll copy the engines into the region at the end.
    containedEngines = {}

    genRegionInSlot = []

    fillRegion @grid, slot0, (slot, hmm) =>
      [x, y, n] = slot
      #console.log x, y, isTop

      k = "#{x},#{y}"
      sk = "#{slot}"
 
      sw2 = @makeSwitchInSlot slot
      return no unless sw2
      r2 = sw2.get shuttleStates
      list = @gridFill[slot]

      if @gridKey[k] != key0
        log 'connection', list0, list
        # Connect us.
        if isListSubsetOf list0, list
          log 'sw2 looks down to me'
          # The other region should look down on us.
          if r2
            r2.down[id] = r
          else
            # Go make it, dagnabbit!
            genRegionInSlot.push slot
        else
          # They look up to us.
          log 'sw2 looks up to me'

        if isListSubsetOf list, list0
          log 'I look down on sw2'
        else
          log 'I look up to sw2'

        return no

      if r2
        throw Error "Overlapping regions at #{[x,y,n]}"

      if list then for sid, i in list by 2
        stateid = list[i+1]
        return no if shuttleStates[sid] is stateid

      sw2.set shuttleStates, r
      r.size++
      
      engine = @engineGrid[k]
      if engine
        containedEngines[engine.id] = engine.pressure

      return yes

    r.engines = containedEngines

    # Recurse...
    @makeRegion r, shuttleStates for r in genRegionInSlot

    r

   
  step: ->
    log '####### STEP #######'
    disturbedShuttles = {}

    # 1. Update the zones
    for r in @genZoneForRegion when !r.zone?.used
      zone =
        regions: [r]
        engines: {}
        pressure: 0
        used: yes

      # some sort of graph flood fill thing here through connections
      r.zone = zone
      for id,pressure of r.engines
        if !zone.engines[id]
          zone.engines[id] = pressure
          zone.pressure += pressure

      for {s} in r.appliesPressureTo
        disturbedShuttles[s.id] = s

      log r.zone

    @genZoneForRegion.length = 0

    # 2. Move shuttles
    for id,s of disturbedShuttles
      console.log 'calc', id

      state = s.states[s.currentid]
      flags = state.flags

      moved = no
      forces = @makeStateForces(s, state).get @shuttleState
      throw Error 'Could not find forces' unless forces
      log 'force', forces

      if flags & ((1<<UP)|(1<<DOWN))
        impulse = 0
        console.log 'checking up/down force'
        for rid,mult of forces.y
          zone = @regions[rid].zone
          impulse -= zone.pressure * mult

        if impulse
          dir = if impulse < 0 then UP else DOWN
          forceMag = abs impulse

          while forceMag
            break unless @tryMoveShuttle s, dir
            moved = yes
            forceMag--

          if flags & 1<<dir
            console.log 'force', impulse, dir

    # 3. Regenerate invalidated regions
    console.log 'invalid regions:', @invalidRegions.length
    for {r, x, y} in @invalidRegions when r.used
      console.log x, y
      r.used = false

    @invalidRegions.length = 0

    # 4. Recalculate invalidated shuttle forces





  tryMoveShuttle: (s, dir) ->
    log 'trying to move', s, dir
    #s = @shuttles[sid]
    state = s.states[s.currentid]
    return no if !state.flags & (1<<dir)

    successorid = state.successor[dir]
    if successorid is -1
      # Try to make a new state for the shuttle.
      log 'Found new state for shuttle', s
      d = dirs[dir]
      successorid = @addShuttleState s, state.dx + d.dx, state.dy + d.dy
      state.successor[dir] = successorid
      successor = s.states[successorid]
      log s.states, successorid
      successor.successor[oppositeDir dir] = s.currentid
      log 'made new state', successor

    # TODO: Test that we don't overlap with any other shuttles
    # TODO: Call shuttle movement callback

    console.log 'moving from', s.currentid, 'to', successorid
    state.currentid = successorid

    yes









    
parseFile = exports.parseFile = (filename, opts) ->
  fs = require 'fs'
  data = JSON.parse fs.readFileSync(filename, 'utf8').split('\n')[0]
  # Mmmm, resiliancy.
  delete data.tw
  delete data.th
  jit = new Jit data, opts
  
  jit.step()
  log '##########'
  log jit.gridFill
  log jit.shuttles[0].states
  #jit.step()


if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename



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

log = (args...) ->
  if require
    {inspect} = require 'util'
    f = (a) -> if typeof a is 'string' then a else inspect a, {depth:5, colors:true}
    console.log args.map(f).join ' '
  else
    console.log args...

#log {x:[]}, [1,2,3], "hi there"

{parseXY, fill} = util = require './util'

UP=0; RIGHT=1; DOWN=2; LEFT=3
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
  
  cellAt: (x, y, dir) ->
    v = @get x, y
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

  cellOpposite: (x, y, dir) ->
    {dx, dy} = dirs[dir]
    @cellAt x+dx, y+dy, oppositeDir(dir)

  # ********
 

  constructor: (@grid, @opts = {}) ->
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
    # an unexpected way. Zones are recreated when there's any change to the connectivity graph.
    @regions = {}
    @zones = {}

    # This maps x,y of cells to a list of shuttle/state pairs which we know can
    # go in the grid cell.
    @gridFill = {}
    # This is a map from x,y to a string key of which shuttles the cell is dependant on
    @gridKey = {}

    # map from grid position -> engine
    @engineGrid = {}

    # map from grid position -> list of shuttles which can occupy that position.
    @shuttleGrid = {}
    
    # map from "x,y[,slot]" to region. Slots are directions for engines, [0,1]
    # for bridges, unused for thinsolid / thinshuttle.
    @regionInSlot = {}
    # map from x,y -> region which filled through that point.
    #@regionGrid = {}


    @killRegionInSlot = []
    @genRegionInSlot = []

    @genZoneForRegion = []


    # set of edges which need a region filled out.
    #@dirtyEdges = {}

    # set of regions which need a derived region
    #@dirtyRegions = {}


    @initialize()

  initialize: ->
    # Scan for all engines, shuttles.
    @annotateGrid()

    log @genRegionInSlot

    @makeRegionInSlot slot while slot = @genRegionInSlot.pop()
    @genRegionInSlot.length = 0

    #@fillRegions()

    for s in @shuttles
      @calcStateForces s, state for state in s.states
    

    log 'engines', @engines
    log 'shuttles', @shuttles
    log @regions, @regionGrid
    ###

    log @regionInSlot
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

          if @fillMode in ['engines', 'all']
            for d in [0...4]
              @genRegionInSlot.push [x,y,d]

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

                if @fillMode in ['shuttles', 'all']
                  cell = @cellOpposite x, y, dir
                  @genRegionInSlot.push cell if cell isnt null

              true
            else
              false

          @addShuttleState s, 0, 0

  shuttleFillsCell: (x, y, s, stateid) ->
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
      # This is going to do a lexographic sort, but it doesn't really matter so long as its stable.
      @gridKey[k] = Object.keys(sids).sort().join ','

    if r = @regionInSlot[k]
      @killRegionInSlot.push {r, x, y}

  addShuttleState: (s, dx, dy) ->
    # successor is the connected state in each direction
    state = {dx, dy, flags:0, successor: [-1,-1,-1,-1]}
    stateid = s.states.length
    s.states.push state

    for k,v of s.points when v is 'shuttle'
      {x,y} = parseXY k
      @shuttleFillsCell x+dx, y+dy, s, stateid

    # Find which directions the shuttle can move in from state 0 (here)
    @calcShuttleDirs s, state

    #console.log 'state', state
    stateid

  calcStateForces: (s, state) ->
    {dx, dy, flags} = state
    xForce = {}
    yForce = {}
    for {x,y,isTop,dir} in s.edge
      slot = @cellOpposite x, y, dir
      continue unless slot
      r = @regionInSlot[slot]
      # If r is missing, we should be up against a wall or something. No
      # biggie.
      continue unless r

      r.appliesPressureTo.push {s,state}

      if isTop
        yForce[r.id] ?= 0
        yForce[r.id] += dirs[dir].dy
      else
        xForce[r.id] ?= 0
        xForce[r.id] += dirs[dir].dx

    # This could probably be munged into a slightly more useful form, but eh.
    state.force = {x:xForce, y:yForce}


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

  ###
  fillRegions: ->
    # Flood fill all the empty regions in the grid
    for k,v of @grid when !@regionGrid[k]
      {x,y} = parseXY k

      # We'll skip making regions when the region is between two engines, or an
      # engine and a wall or something.
      for {ex,ey,isTop},dir in edges
        {dx, dy} = dirs[dir]
        v2 = @get x+dx, y+dy
        # We're interested in every edge so long as both grid cells aren't
        # walls, and we're not in the middle of two shuttles.
        continue if !v2? || (v == 'shuttle' and v2 == 'shuttle')

        @makeRegionFrom(x+ex, y+ey, isTop)
  ###

  makeRegionInSlot: (slot) ->
    return null if slot is null
    r = @regionInSlot[slot]
    return r if r

    [x0, y0, n0] = slot

    # Does it make sense to have a region here? Maybe this is a pointless optimization...
    v0 = @get x0, y0
    if v0 in ['positive', 'negative']
      {dx, dy} = dirs[n0]
      return null if !@get(x0+dx, y0+dy)


    id = "r#{@id()}"
    key = @gridKey["#{x0},#{y0}"]

    
    console.log "creating region #{id} from [#{slot.join ','}] with key #{key}"
    @regions[id] = r =
      engines: []
      connections: null
      size: 0 # Just for debugging.
      appliesPressureTo: []
      id: id
      key: key
      used: yes
      zone: null

    @genZoneForRegion.push r

    toExplore = []
    visited = {}
    connections = {}

    # We'll hit the same engine multiple times. Using a map to dedup, then
    # we'll copy the engines into the region at the end.
    containedEngines = {}

    hmm = (slot) =>
      return unless slot
      sk = slot.join ','
      if !visited[sk]
        #r2 = @regionInSlot[sk]
        #if r2 and r2.used
        #  throw Error "Overlapping regions at #{sk}"
          
        #console.log 'expanding', r, 'to', x, y, isTop
        visited[sk] = true
        toExplore.push slot

    hmm slot

    while slot = toExplore.pop()
      [x, y, n] = slot
      #console.log x, y, isTop

      k = "#{x},#{y}"
      sk = "#{slot}"
 
      if @gridKey[k] != key
        r2 = @regionInSlot[sk]
        if r2?.used
          # Connect us.
          connections[r2.id] = r2
          r2.connections[id] = r
        else
          # There *should* be a region here.
          @genRegionInSlot.push slot
        continue

      if @regionInSlot[sk]
        throw Error "Overlapping regions at #{[x,y,n]}" if r2.key is key

      @regionInSlot[sk] = r
      v = @get x, y

      # We need to check for connectivity via all adjoining cells
      switch v
        when 'bridge'
          r.size++
          if n is 0
            hmm @cellOpposite x, y, UP
            hmm @cellOpposite x, y, DOWN
          else
            hmm @cellOpposite x, y, LEFT
            hmm @cellOpposite x, y, RIGHT
        when 'nothing', 'thinsolid', 'thinshuttle', 'shuttle'
          r.size++
          hmm @cellOpposite x, y, d for d in [0...4]
        when 'positive', 'negative'
          containedEngines[@engineGrid["#{x},#{y}"].id] = pressureOf v
          hmm @cellOpposite x, y, n

    r.engines = containedEngines
    r.connections = connections
    ###
    for eid, pressure of containedEngines
      eid = eid|0
      r.engines.push eid
      r.pressure += pressure
      @engines[eid].regions.push rid
    ###

    r

    
  ###
  recalcDerivedRegions: ->
    for id of @dirtyRegions
      r = @regions[id]
      # Flood fill using r's connections
      console.log 'refilling', id
  ###

  step: ->
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
      if flags & ((1<<UP)|(1<<DOWN))
        force = 0
        console.log 'checking up/down force'
        for rid,mult of state.force.y
          zone = @regions[rid].zone
          force -= zone.pressure * mult

        if force
          dir = if force < 0 then UP else DOWN
          forceMag = abs force

          while forceMag
            break unless @tryMoveShuttle s, dir
            moved = yes
            forceMag--

          if flags & 1<<dir
            console.log 'force', force, dir

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



        #if (force < 0 && (flags & (1<<UP))) || (force > 0 && (flags & (1<<DOWN)))









    
parseFile = exports.parseFile = (filename, opts) ->
  fs = require 'fs'
  data = JSON.parse fs.readFileSync(filename, 'utf8').split('\n')[0]
  # Mmmm, resiliancy.
  delete data.tw
  delete data.th
  jit = new Jit data, opts
  
  #jit.step()
  log '##########'
  log jit.gridFill
  log jit.shuttles[0].states
  #jit.step()


if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename



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
    console.log args.map((a) -> inspect a, {depth:5, colors:true}).join ' '
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

oppositeDir = (dir) -> dir+2 % 4

pressureOf = (v) -> if v is 'positive' then 1 else -1

abs = (x) -> if x < 0 then -x else x

class Jit
  constructor: (@grid, @opts = {}) ->
    @fillMode = @opts.fillMode || 'engines'

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

    # map from grid position -> engine
    @engineGrid = {}

    # map from grid position -> list of shuttles which can occupy that position.
    @shuttleGrid = {}
    
    # map from "x,y,isTop" to region
    @regionAtEdge = {}
    # map from x,y -> region which filled through that point.
    @regionGrid = {}



    @invalidRegions = []

    @unzonedRegions = []


    # set of edges which need a region filled out.
    #@dirtyEdges = {}

    # set of regions which need a derived region
    #@dirtyRegions = {}


    @initialize()

  id: -> @nextId++
  get: (x,y) -> @grid["#{x},#{y}"]
  #addDirtyEdge: (x, y, isTop) -> @dirtyEdges["#{x},#{y},#{isTop}"] = {x, y, isTop}

  ###
  addDirtyEdgesBounding: (x, y, extra) ->
    for {ex, ey, isTop, dx, dy} in edges when (
        @get(x+dx,y+dy) in ['nothing', 'thinsolid', 'thinshuttle', 'bridge', extra])
      @addDirtyEdge x+ex, y+ey, isTop
  ###

  initialize: ->
    # Scan for all engines, shuttles.
    @annotateGrid()

    @fillRegions()

    for s in @shuttles
      @calcStateForces s, state for state in s.states
    

    log @engines
    log @shuttles
    ###
    log @regions, @regionGrid

    log @regionAtEdge
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

          #@addDirtyEdgesBounding x, y, 'shuttle' if @fillMode in ['all', 'engines']

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

              true
            else
              false

          @addShuttleState s, 0, 0

  shuttleFillsCell: (x, y, s, stateid) ->
    k = "#{x},#{y}"
    if !@gridFill[k]
      @gridFill[k] = [s.id, stateid]
    else
      list = @gridFill[k]
      # We already know that the shuttle fills the cell sometimes. No need to
      # get huffy about it.
      return for sid, i in list when sid is s.id and list[i+1] is stateid
      list.push s.id
      list.push stateid

    if r = @regionGrid[k]
      @invalidRegions.push {r, x, y}
      #@invalidateRegion r, x, y

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
      r = @regionAtEdge["#{x+dx},#{y+dy},#{isTop}"]
      # If r is missing, we're probably up against a wall or something. No
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

  makeRegionFrom: (x, y, isTop) ->
    r = @regionAtEdge["#{x},#{y},#{isTop}"]
    return if r isnt undefined

    id = "r#{@id()}"
    
    console.log "creating region #{id} from #{x}, #{y}, #{isTop}"
    @regions[id] = r =
      engines: []
      connections: {}
      size: 0
      used: yes
      appliesPressureTo: []
      id: id
      zone: null

    @unzonedRegions.push r

    #@dirtyRegions[id] = true

    toExplore = []
    visited = {}

    # We'll hit the same engine multiple times. Using a map to dedup, then
    # we'll copy the engines into the region at the end.
    containedEngines = {}

    hmm = (x, y, isTop) =>
      ek = "#{x},#{y},#{isTop}"
      if !visited[ek]
        throw Error "Overlapping regions at #{ek}" if @regionAtEdge[ek]?.used
        #console.log 'expanding', r, 'to', x, y, isTop
        visited[ek] = true
        toExplore.push {ek,x,y,isTop}

    hmm x, y, isTop

    while n = toExplore.shift()
      {ek,x,y,isTop} = n
      #console.log x, y, isTop

      # We need to check for connectivity via the two adjoining grid cells.
      #
      # We need:
      # - x,y of the cell to check
      # - ox,oy is the opposite edge via that cell for when we're calculating
      # bridges.
      # - If we hit a shuttle, we need to know which way the force pushes
      check = if isTop # Above, below
        [{x, y:y-1, ox:x, oy:y-1, f:UP}, {x, y, ox:x, oy:y+1, f:DOWN}]
      else # Left, right
        [{x:x-1, y, ox:x-1, oy:y, f:LEFT}, {x, y, ox:x+1, oy:y, f:RIGHT}]

      # Don't smear regions up against walls. Its unsightly.
      for c in check
        {x,y} = c
        c.k = "#{x},#{y}"
        c.v = @grid[c.k]
      continue if !check[0].v || !check[1].v

      @regionAtEdge[ek] = r

      for {x,y,k,v,ox,oy}, i in check
        # unnecessary, but dramatically speeds up the churn here.
        continue if @regionGrid[k]

        switch v
          when 'bridge'
            r.size++
            hmm ox, oy, isTop
          when 'nothing', 'thinsolid', 'thinshuttle'
            r.size++
            hmm x, y, true
            hmm x, y, false
            hmm x, y+1, true
            hmm x+1, y, false
          when 'positive', 'negative'
            containedEngines[@engineGrid["#{x},#{y}"].id] = pressureOf v
          else
            continue

        console.log 'm', k
        @regionGrid[k] = r

    r.engines = containedEngines
    ###
    for eid, pressure of containedEngines
      eid = eid|0
      r.engines.push eid
      r.pressure += pressure
      @engines[eid].regions.push rid
    ###

    r

  
  invalidateRegion: (r, x, y) ->
    console.error 'invalidate region', r
    r.used = no
    #for {s, state} of
    
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
    for r in @unzonedRegions when !r.zone?.used
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

    @unzonedRegions.length = 0

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

    # 3. Regenerate regions
    console.log 'invalid regions:', @invalidRegions.length
    for {r, x, y} in @invalidRegions when r.used
      console.log x, y
      r.used = false


    @invalidRegions.length = 0



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
  
  jit.step()
  log '##########'
  log jit.gridFill
  #jit.step()


if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename



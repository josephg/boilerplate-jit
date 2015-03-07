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

dirs =
  up: {dx:0, dy:-1, flag:1}
  right: {dx:1, dy:0, flag:2}
  down: {dx:0, dy:1, flag:4}
  left: {dx:-1, dy:0, flag:8}

# up, right, down, left
edges = [
  {ex:0,ey:0,isTop:true,dx:0,dy:-1}
  {ex:1,ey:0,isTop:false,dx:1,dy:0}
  {ex:0,ey:1,isTop:true,dx:0,dy:1}
  {ex:0,ey:0,isTop:false,dx:-1,dy:0}
]


pressureOf = (v) -> if v is 'positive' then 1 else -1

class Jit
  constructor: (@grid, @opts = {}) ->
    @fillMode = @opts.fillMode || 'engines'

    console.log @grid
    @extents = util.gridExtents @grid
    util.printGrid @extents, @grid

    @nextId = 100

    # The engines and shuttles won't change often - but the regions are deleted
    # and recreated whenever a shuttle nearby moves in an unexpected way.
    @engines = {}
    @regions = {}
    @derivedRegions = {}
    @shuttles = {}


    # map from grid position -> engine
    @engineGrid = {}

    @shuttleGrid = {}
    
    # map from "x,y,isTop" to region
    @regionGrid = {}

    # set of edges which need a region filled out.
    @dirtyEdges = {}

    # set of regions which need a derived region
    @dirtyRegions = {}


    @initialize()

  id: -> @nextId++
  get: (x,y) -> @grid["#{x},#{y}"]
  addDirtyEdge: (x, y, isTop) -> @dirtyEdges["#{x},#{y},#{isTop}"] = {x, y, isTop}

  addDirtyEdgesBounding: (x, y, extra) ->
    for {ex, ey, isTop, dx, dy} in edges when (
        @get(x+dx,y+dy) in ['nothing', 'thinsolid', 'thinshuttle', 'bridge', extra])
      @addDirtyEdge x+ex, y+ey, isTop

  initialize: ->
    # Scan for all engines, shuttles.
    @annotateGrid()

    @fillRegions()

    console.log @engines
    console.log @shuttles
    console.log @regions, @regionGrid


  annotateGrid: ->
    for k,v of @grid when k not in ['tw', 'th']
      {x,y} = parseXY k
      switch v
        when 'positive', 'negative'
          # Mark these - we'll need them later.
          pressure = pressureOf v
          id = @id()
          engine = {x, y, pressure, regions:[], id}
          @engineGrid["#{x},#{y}"] = @engines[id] = engine

          @addDirtyEdgesBounding x, y, 'shuttle' if @fillMode in ['all', 'engines']

        when 'shuttle', 'thinshuttle'
          # flood fill the shuttle extents.
          continue if @shuttleGrid["#{x},#{y}"]?

          id = @id()
          @shuttles[id] = s =
            points: {} # Grid of points in the shuttle in the base state
            states: [] # List of the {dx,dy,pushedBy} of each state

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

              @shuttleGrid["#{x},#{y}"] = s
              s.points["#{x},#{y}"] = v
              true
            else
              false

          @addShuttleState s, 0, 0


  addShuttleState: (s, dx, dy) ->
    state = {dx, dy, flags:0}
    s.states.push state

    # Find which directions the shuttle can move in from state 0 (here)
    @calcShuttleDirs s, state

    console.log 'state', state

  calcShuttleDirs: (s, state) ->
    flags = 0
    for _, {dx, dy, flag} of dirs
      if @canShuttleMoveTo s, dx + state.dx, dy + state.dy
        flags |= flag
      state.flags = flags

    return

  canShuttleMoveTo: (s, dx, dy) ->
    # Can the specified shuttle be in the specified dx/dy positions?
    for k of s.points
      {x, y} = parseXY k
      x += dx; y += dy
      continue if @shuttleGrid["#{x},#{y}"] is s
      continue if @get(x, y) is 'nothing'
      return no
    return yes


  # This will happen for all tiles which aren't engines and aren't in shuttle zones
  # (so, engines, empty space, grills and bridges)
  letsAirThrough =
    nothing: yes
    thinsolid: yes
    bridge: yes
    thinshuttle: yes

  fillRegions: ->
    # Flood fill all the empty regions in the grid
    for k,v of @grid when letsAirThrough[v]
      {x,y} = parseXY k

      #continue if @shuttleGrid[k]

      # We'll skip making regions when the region is between two engines, or an
      # engine and a wall or something.
      @makeRegionFrom(x+ex, y+ey, isTop) for {ex,ey,isTop,dx,dy} in edges when (
          letsAirThrough[v] ||
          letsAirThrough[@get(x+dx, y+dy)] ||
          @shuttleGrid["#{x+dx},#{y+dy}"] != undefined)

  makeRegionFrom: (x, y, isTop) ->
    k = "#{x},#{y},#{isTop}"
    r = @regionGrid[k]
    return if r isnt undefined

    id = @id()
    
    @regions[id] = r =
      engines: []
      connections: {}
      size: 0

    @dirtyRegions[id] = true

    toExplore = []
    visited = {}

    # We'll hit the same engine multiple times. Using a map to dedup, then
    # we'll copy the engines into the region at the end.
    containedEngines = {}

    hmm = (x, y, isTop) =>
      k = "#{x},#{y},#{isTop}"
      if visited[k] is undefined and @regionGrid[k] is undefined
        #console.log 'expanding', r, 'to', x, y, isTop
        visited[k] = @regionGrid[k] = r
        toExplore.push {x,y,isTop}

    hmm x, y, isTop

    while n = toExplore.shift()
      {x,y,isTop} = n
      #console.log x, y, isTop

      # We need to check for connectivity via the two adjoining grid cells.
      #
      # We need:
      # - x,y of the cell to check
      # - ox,oy is the opposite edge via that cell for when we're calculating
      # bridges.
      # - If we hit a shuttle, we need to know which way the force pushes
      check = if isTop # Above, below
        [{x, y:y-1, ox:x, oy:y-1, f:dirs.up}, {x, y, ox:x, oy:y+1, f:dirs.down}]
      else # Left, right
        [{x:x-1, y, ox:x-1, oy:y, f:dirs.left}, {x, y, ox:x+1, oy:y, f:dirs.right}]

      for {x,y,ox,oy,f}, i in check
        k = "#{x},#{y}"
        sid = @shuttleGrid[k]
        v = @grid[k]

        if sid != undefined
          # This is the boundary with a shuttle. Mark it - we'll come back in
          # the next pass.
          #r.tempEdges.push {x,y,sid,f}
          continue

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

    ###
    for eid, pressure of containedEngines
      eid = eid|0
      r.engines.push eid
      r.pressure += pressure
      @engines[eid].regions.push rid
    ###

    r

  recalcDerivedRegions: ->
    for id of @dirtyRegions
      r = @regions[id]
      # Flood fill using r's connections
      console.log 'refilling', id


  step: ->
    @recalcDerivedRegions()




    
parseFile = exports.parseFile = (filename, opts) ->
  fs = require 'fs'
  data = JSON.parse fs.readFileSync(filename, 'utf8').split('\n')[0]
  # Mmmm, resiliancy.
  delete data.tw
  delete data.th
  jit = new Jit data, opts
  
  jit.step()


if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename



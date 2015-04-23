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
log = require './log'


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



oppositeDir = (dir) -> (dir+2) % 4

pressureOf = (v) -> if v is 'positive' then 1 else -1

abs = (x) -> if x < 0 then -x else x

class Grid
  constructor: ->
    @rows = new Map
    @observers = []

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

    if oldValue != v then o(x, y, v) for o in @observers

  each: (fn) ->
    @rows.forEach (row, x) ->
      row.forEach (v, y) ->
        fn x, y, v

  observe: (fn) -> @observers.push fn
  
  depFor: (x, y) ->
    row = @rows.get x
    if !row
      row = new Map
      @rows.set x, row

    {map:row, key:y}


######## Dependancy tree tracking

listeners = new WeakMap

depStore = do ->
  # Maps from object -> list of things it generates
  depRoot = new WeakMap

  # Return any dependancies for the given map/key pair
  getAndClear: (map, key) ->
    container = depRoot.get map
    return if !container

    deps = container.get key
    container.delete key
    deps

  add: (source, result) -> # source.map[source.key] used to make result.map[result.key]
    container = depRoot.get source.map
    if !container
      container = new Map
      depRoot.set source.map, container

    currentDeps = container.get source.key
    if currentDeps
      currentDeps.push result
    else
      container.set source.key, [result]


#activeTransaction = null

didChange = ({map, key}) ->
  deps = depStore.getAndClear map, key
  
  if deps then for d in deps
    log 'blowing up', d.key
    if d.map.delete
      d.map.delete d.key
    else
      delete d.map[d.key]
    # And recursively wipe out the whole tree
    didChange d


eachFwd = (obj, observer) ->
  listeners.set obj, observer

  if obj instanceof Map
    obj.forEach (v, k) ->
      observer k, v

    obj.set = (k, v) ->
      Map.prototype.set.call obj, k, v
      observer k, v
  else if obj instanceof Grid
    obj.each (x, y, v) ->
      observer x, y, v
    obj.observe observer

  else
    throw Error 'eachFwd not implemented on obj'


setWithDeps = (map, key, val, sourceDeps) ->
  #log 'map', map, map.constructor

  if map.set
    map.set key, val
  else
    map[key] = val

  for d in sourceDeps
    depStore.add d, {map, key}

###
setWithDeps @engines, [x,y], {v:1}, [{@grid, [x,y]}]

  
###

######### JIT

class Jit
  id: -> @nextId++

  constructor: (rawGrid, @opts = {}) ->
    util.printGrid (util.gridExtents rawGrid), rawGrid
    @nextId = 1

    @grid = new Grid()
    for k, v of rawGrid when k not in ['tw', 'th']
      {x,y} = parseXY k
      @grid.set x, y, v

    @regionAtCell = new Grid()
    @regions = {}



    @initialize()


  initialize: ->
    @grid.observe (x, y) => didChange @grid.depFor x, y

    eachFwd @grid, (x, y, v) =>
      return unless v

      if v in ['nothing', 'thinshuttle']
        return if @regionAtCell.get x, y

        id = "r#{@id()}"
        console.log "making #{id} at", x, y

        r =
          id: id
          size: 0

        deps = []

        fill {x,y}, (x, y) =>
          v = @grid.get x, y
          deps.push @grid.depFor x, y
          if v in ['nothing', 'thinshuttle']
            @regionAtCell.set x, y, r
            r.size++

            yes
          else
            no

        #log 'deps', deps
        #@regions[id] = r
        setWithDeps @regions, id, r, deps

    log @regions

    @grid.set 1, 2, null

    log 'rrrrr', @regions







    
parseFile = exports.parseFile = (filename, opts) ->
  fs = require 'fs'
  data = JSON.parse fs.readFileSync(filename, 'utf8').split('\n')[0]
  # Mmmm, resiliancy.
  delete data.tw
  delete data.th
  jit = new Jit data, opts
  


if require.main == module
  filename = process.argv[2]
  throw Error 'Missing file argument' unless filename
  parseFile filename



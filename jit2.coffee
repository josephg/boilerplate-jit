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

# Set map[key] = val
set = (map, key, val) ->
  if map.set # Map / WeakMap
    if val?
      map.set key, val
    else
      map.delete key
  else if map.add # Set / WeakSet
    if val?
      map.add val
    else
      map.delete val
  else # Object
    if val?
      map[key] = val
    else
      delete map[key]

setWithDeps = (map, key, val, sourceDeps) ->
  #log 'map', map, map.constructor
  set map, key, val

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

  regionDestroy: (r, x, y) ->
    return unless r?.used
    log "blowing up region #{r.id}"
    r.used = no
    delete @regions[r.id]
    didChange {map:@regions, key:r.id}

    if x?
      for {dx, dy} in dirs
        @genRegions x+dx, y+dy

  genRegions: (x, y) =>
    v = @grid.get x, y
    return unless v

    if v in ['nothing', 'thinshuttle']
      return if @regionAtCell.get(x, y)?.used

      id = "r#{@id()}"
      console.log "making #{id} at", x, y

      r =
        id: id
        size: 0
        used: yes

      #deps = []

      fill {x,y}, (x, y) =>
        v = @grid.get x, y
        #deps.push @grid.depFor x, y
        if v in ['nothing', 'thinshuttle']
          @regionAtCell.set x, y, r
          r.size++

          yes
        else
          no

      #log 'deps', deps
      @regions[id] = r
      #setWithDeps @regions, id, r, deps


  initialize: ->
    @grid.observe (x, y, v) =>
      didChange @grid.depFor x, y

      if v is 'nothing'
        for {dx, dy} in dirs
          r = @regionAtCell.get x+dx, y+dy
          if r then @regionDestroy r
        @genRegions x, y
      else
        r = @regionAtCell.get x, y
        @regionDestroy r, x, y

    eachFwd @grid, @genRegions

    log @regions

    #@grid.set 1, 1, 'nothing'
    @grid.set 1, 3, 'nothing'

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



assert = require 'assert'
log = require './log'


inTransaction = 0
sentSignals = 0

exports.Watcher = class Watcher
  constructor: (@forEach) ->
    @observers = []

  on: (fn) ->
    @observers.push fn

  signal: (args...) ->
    sentSignals++
    o(args...) for o in @observers
    return


invariants = []



# data

data = new Map
data.watch = new Watcher

data.set2 = (k, v) ->
  @set k, v
  @watch.signal k #, @get k

data.del2 = (k) ->
  @delete k
  @watch.signal k

# ********** data -> regions

regions = new Map
regions.watch = new Watcher

regions.get2 = (k) ->
  v = @get k
  if v is null or v?.used
    v
  else
    @gen k

regions.forEach2 = (fn) ->
  set = new Set
  data.forEach (v, k) =>
    r = @get2 k
    set.add r if r?.used

  set.forEach fn


data.watch.on (x) ->
  log 'got signal', x
  destroyRegion x-1
  destroyRegion x
  destroyRegion x+1

destroyRegion = (x) ->
  r = regions.get x
  if r?.used
    r.used = no
    log 'destroyRegion', r
    regions.watch.signal r, false

regions.gen = (x) ->
  log 'gen at', x
  #return r if (r = @get(x)?.used)
  return null if !data.get x

  min = max = x
  min-- while data.get(min - 1)
  max++ while data.get(max + 1)

  r = {min, max, used:yes}
  @set i, r for i in [min..max]
  log 'generated', r
  regions.watch.signal r, true
  return r

invariants.push ->
  regions.forEach2 (r) ->
    assert r.used
    # Invariant: All regions show live cells
    for i in [r.min..r.max]
      assert data.get i
    
    # Invariant: No two regions are adjacent to one another
    r2 = regions.get2 r.min - 1
    assert !r2?.used
    r2 = regions.get2 r.max + 1
    assert !r2?.used

  data.forEach (v, x) ->
    # Invariant: Each live cell has a matching region
    assert.equal !!v, !!regions.get2(x)?.used



# ******* regions -> connections
connections = new Map # region -> set of regions
connections.watch = new Watcher

incompleteRegions = new Set

connections.forEach2 = (fn) ->

regions.watch.on (r, created) ->
  if created
    incompleteRegions.add r
    incompleteRegions.add r2 if (r2 = regions.get r.min-2)?.used
    incompleteRegions.add r2 if (r2 = regions.get r.max+2)?.used
  else
    log 'removed region', r
    incompleteRegions.delete r
    set = connections.get r
    if set
      connections.delete r
      set.forEach (r2) ->
        rmConnection r2, r

rmConnection = (r1, r2) ->
  log 'rm connection', r1, r2
  set = connections.get r1
  #if set?.delete r2
  #  connections.watch.signal r1, r2
  connections.delete r1 if set.size is 0

connections.get2 = (r) ->
  cs = @get r
  if cs and !incompleteRegions.has cs
    cs
  else
    @gen r

connections.gen = (r) ->
  # Generate connections for region r
  log 'new region', r
  # Look for regions on either side
  r2 = regionForCell.get r.min - 2
  if r2?.used
    # Add a connection!
    addConnection r, r2
    addConnection r2, r
  
  r2 = regionForCell.get r.max + 2
  if r2?.used
    # Add a connection!
    addConnection r, r2
    addConnection r2, r
  incompleteRegions.delete r

addConnection = (r1, r2) ->
  log 'add connection', r1, r2
  set = connections.get r1
  connections.set r1, (set = new Set) unless set
  if !set.has r2
    set.add r2
    connections.watch.signal r1, r2

invariants.push ->
  connections.forEach (set, r1) ->
    set.forEach (r2) ->
      # Invariant: All connections refer to active regions
      assert r1.used
      assert r2.used

      # Invariant: All connections are at least 1 apart
      assert (r1.max == r2.min - 2) or (r2.max == r1.min - 2)

  # ... And Invariant: There exists a connection between any adjacent regions
  # You get the point.

connections.watch.on (r1, r2) ->
  log 'XX connection from', r1, r2


###
data.set2 1, true
data.set2 2, true
data.set2 3, true
data.set2 4, true
data.set2 5, true

log data
log regions

regions.forEach2 (r) -> log r
log 'xxx'
data.set2 3, false
regions.forEach2 (r) -> log r

data.del2 2
data.del2 3

regions.forEach2 (r) -> log r

log regionForCell
connections.forEach (s, r) ->
  log 'connections for', r, s
###

mersenne = require 'mersenne'
mersenne.seed 123

for [1...1000]
  for [1..5]
    v = mersenne.rand() % 5
    data.set2 v, !!(mersenne.rand() % 2)

  try
    for fn in invariants
      fn()

  catch e
    log data
    log 'regions:'
    regions.forEach2 (r) -> log r

    #connections.forEach (s, r) ->
    #  log 'connections for', r, s

    throw e


log data
log 'regions:'
regions.forEach2 (r) -> log r

console.log 'ss', sentSignals

assert = require 'assert'

invariants = []

###
inTransaction = 0
pendingWatchers = new Set

txn = (fn) ->
  inTransaction++
  fn()
  inTransaction--
  if !inTransaction
    while pendingWatchers.size
      log 'flushing all', data
      log regionForCell
      pendingWatchers.forEach (watch) ->
        watch.flush()

Watcher = (forEach) ->
  observers = []
  heldSignals = []

  on: (fn) ->
    forEach fn
    observers.push fn

  signalImm: (a, b, c) ->
    o(a,b,c) for o in observers

  flush: ->
    pendingWatchers.delete this
    # Danger - if it signals itself, we might not notice.
    @signalImm a,b,c for [a,b,c] in heldSignals
    heldSignals.length = 0

  signal: (a,b,c) ->
    if inTransaction
      heldSignals.push [a,b,c]
      pendingWatchers.add this
    else
      @signalImm a,b,c
###

{Watcher, txn} = require './watch'

log = require './log'

# data

data = new Map
data.watch = new Watcher (fn) ->
  data.forEach (v, k) -> fn k, v

data.set2 = (k, v) ->
  @set k, v
  @watch.signal k, v

data.del2 = (k) ->
  @delete k
  @watch.signalImm k

# data -> regions

regionForCell = new Map

regionWatch = new Watcher (fn) ->
  mark = new WeakSet
  regionForCell.forEach (r) ->
    if !mark.has r
      mark.add r
      fn r

destroyRegion = (x) ->
  r = regionForCell.get x
  if r
    r.used = no
    log 'destroyRegion', r
    regionWatch.signalImm r

gen = (x) ->
  log 'gen at', x
  return if regionForCell.get(x)?.used
  return if !data.get x

  min = max = x
  while data.get(min - 1)
    min--
    destroyRegion min # I only need this because of pesky transactions :p
  while data.get(max + 1)
    max++
    destroyRegion max

  r = {min, max, used:yes}
  regionForCell.set i, r for i in [min..max]
  log 'generated', r
  regionWatch.signal r

data.watch.on (x, v) ->
  log 'got signal', x, v, data
  #return if !!regionForCell.get(x)?.used == !!v # Commented out to make txns work
  return if v != data.get(x)

  v = data.get x

  if v
    destroyRegion x-1
    destroyRegion x+1
    gen x
  else
    destroyRegion x
    gen x-1
    gen x+1

invariants.push ->
  regionForCell.forEach (r) ->
    return unless r.used
    # Invariant: All regions show live cells
    for i in [r.min..r.max]
      assert data.get i
    
    # Invariant: No two regions are adjacent to one another
    r2 = regionForCell.get r.min - 1
    assert !r2?.used
    r2 = regionForCell.get r.max + 1
    assert !r2?.used

  data.forEach (v, x) ->
    # Invariant: Each live cell has a matching region
    assert.equal !!v, !!regionForCell.get(x)?.used

# regions -> connections
connections = new Map # region -> set of regions
connections.watch = new Watcher (fn) ->
  connections.forEach (set, r) ->
    set.forEach (r2) ->
      fn r, r2

addConnection = (r1, r2) ->
  log 'add connection', r1, r2
  set = connections.get r1
  connections.set r1, (set = new Set) unless set
  if !set.has r2
    set.add r2
    connections.watch.signal r1, r2

rmConnection = (r1, r2) ->
  log 'rm connection', r1, r2
  set = connections.get r1
  if set?.delete r2
    connections.watch.signal r1, r2
  connections.delete r1 if set.size is 0

regionWatch.on (r) ->
  if r.used
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

  else
    log 'removed region', r
    set = connections.get r
    if set
      connections.delete r
      set.forEach (r2) ->
        rmConnection r2, r
      connections.delete r if set.size is 0

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
txn ->
  data.set2 1, true
  data.set2 2, true
  data.set2 3, true
  data.set2 4, true
  data.set2 5, true

txn ->
  data.del2 2
  data.del2 3


log regionForCell
connections.forEach (s, r) ->
  log 'connections for', r, s
###

mersenne = require 'mersenne'
mersenne.seed 123

for [1...1000]
  txn ->
    for [1..5]
      v = mersenne.rand() % 5
      data.set2 v, !!(mersenne.rand() % 2)

  try
    for fn in invariants
      fn()

  catch e
    log data
    log regionForCell
    #connections.forEach (s, r) ->
    #  log 'connections for', r, s

    throw e



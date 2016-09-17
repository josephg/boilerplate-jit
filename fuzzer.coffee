mersenne = require 'mersenne'
assert = require 'assert'
Jit = require './jit'
log = require './log'
util = require './util'

mersenne.seed 6

randomValueFrom = (vals) ->
  totalWeight = 0
  totalWeight += vals[i+1] for _, i in vals by 2
  ->
    r = mersenne.rand() % totalWeight
    for v,i in vals by 2
      r -= vals[i+1]
      break if r < 0

    v

randomBase = randomValueFrom [
    null, 2
    'nothing', 10
    'positive', 1
    'negative', 1
    #'shuttle', 1
    #'thinshuttle', 1
    'bridge', 5
  ]

randomShuttleCell = ->
  val = (if mersenne.rand() % 2 then 0x40 else 0x80)
  # 3/4 chance to try and connect.
  for i in [0...4]
    val |= (1<<i) if mersenne.rand() % 4 > 0
  return val

randomShuttle = (bv) ->
  if bv in ['nothing', 'bridge', 'ribbon', 'ribbonbridge'] then randomShuttleCell() else null

grid = {}
#grid[[x,y]] = 'nothing' for x in [1..2] for y in [1..1]
#grid[[2,2]] = 'positive'

#grid = require './test.json'

jit = new Jit grid

SIZE = 4
EXTENTS = {right:SIZE-1, bottom:SIZE-1}

printPressure = (pressureGrid) ->
  return if log.quiet
  util.printCustomGrid EXTENTS, (x, y) ->
    p = pressureGrid[[x,y]]
    if p > 0
      if p > 1 then '^' else '+'
    else if p < 0
      if p < -1 then 'v' else '-'
    else
      ';'

debugPrint = ->
  jit.printGrid()
  console.log JSON.stringify jit.toJSON()

fuzz = ->
  log.quiet = yes
  start = Date.now()
  for iter in [1..10000]
    #log.quiet = false
    debugPrint() unless log.quiet
    #debugPrint() if iter >= 950
    #jit.check()
    for [1..2]
      x = mersenne.rand() % SIZE
      y = mersenne.rand() % SIZE
      bv = randomBase()
      sv = randomShuttle bv
      #console.log 'set', x, y, bv, sv #if iter >= 950

      jit.set x, y, bv, sv

    debugPrint() unless log.quiet

    try
      snapshot = jit.toJSON()


      jit.check()
      moved = jit.step()
      jit.check()

      # Check that if step() returns false nothing changed
      if !moved
        snapshot2 = jit.toJSON()
        assert.deepEqual snapshot, snapshot2

        j2 = new Jit snapshot
        assert !j2.step(), 'World erroneously stable'

      # Check that calling step on a copy does exactly the same thing (the sim should be stable)
      copy = new Jit snapshot
      copy.step()
      assert.deepEqual jit.toJSON(), copy.toJSON()

    catch e
      log.quiet = no
      log "****** CRASH ON ITERATION #{iter} ******"

      ###
      log '---- JIT ----'
      printPressure jitPressure
      log '-------------'
      ###
      debugPrint()
      console.log JSON.stringify jit.toJSON()
      log '----'
      #copy.printGrid()
      throw e

    unless iter % 10000
      end = Date.now()
      console.log "#{end-start}ms ----- iter #{iter}", process.memoryUsage?()
      start = end

  for x in [0...SIZE]
    for y in [0...SIZE]
      jit.set x, y, null, null

  jit.baseGrid.forEach (x, y) ->
    jit.set x, y, null, null

  jit.checkEmpty()

window?.fuzz = fuzz
window?.jit = jit

if process.title is 'node'
  mainStart = Date.now()
  fuzz() for [1..10]
  console.log "total time", Date.now() - mainStart

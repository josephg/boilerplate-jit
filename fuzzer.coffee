mersenne = require 'mersenne'
assert = require 'assert'
Simulator = require 'boilerplate-sim'
{parseXY} = Simulator
Jit = require './jit'
log = require './log'
util = require './util'

mersenne.seed 3
randomValue = do ->
  VALS = [
    null, 2
    'nothing', 10
    'positive', 1
    'negative', 1
    'shuttle', 1
    'thinshuttle', 1
  ]

  totalWeight = 0
  totalWeight += VALS[i+1] for _, i in VALS by 2
  ->
    r = mersenne.rand() % totalWeight
    for v,i in VALS by 2
      r -= VALS[i+1]
      break if r < 0

    v

grid = {}
#grid[[x,y]] = 'nothing' for x in [1..2] for y in [1..1]
#grid[[2,2]] = 'positive'

#grid = require './test.json'

sim = new Simulator grid
jit = new Jit grid

SIZE = 3
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
  util.printGrid EXTENTS, jit.grid

#log.quiet = true
fuzz = ->
  start = Date.now()
  for iter in [1..10000]
    #log.quiet = false if iter is 351
    #debugPrint()
    #jit.check()
    for [1...2]
      x = mersenne.rand() % SIZE
      y = mersenne.rand() % SIZE
      v = randomValue()
      log 'set', x, y, v

      sim.set x, y, v
      jit.grid.set x, y, v

    #debugPrint()

    try
      simPressure = sim.getPressure()

      jitPressure = {}
      jit.groups.forEach (group) ->
        zone = jit.zones.getZoneForGroup group
        return unless zone
        #log 'zone', zone.pressure #, zone, group
        if zone.pressure
          #log 'zone with pressure:', zone.pressure, zone
          group.points.forEach (x, y) ->
            #log 'p', x, y, zone.pressure
            jitPressure[[x,y]] = zone.pressure

      for k, v of simPressure
        delete simPressure[k] if v is 0
        if sim.grid[k] in ['positive', 'negative']
          delete simPressure[k]
          delete jitPressure[k]
      assert.deepEqual jitPressure, simPressure

      #sim.step()
      #jit.step()

    catch e
      log '****** CRASH ******'

      log 'jitPressure', jitPressure
      log 'simPressure', simPressure

      log '---- JIT ----'
      printPressure jitPressure
      log '---- SIM ----'
      printPressure simPressure
      log '-------------'

      debugPrint()
      throw e

    unless iter % 10000
      end = Date.now()
      console.log "#{end-start}ms ----- iter #{iter}", process.memoryUsage?()
      start = end

  for x in [0...SIZE]
    for y in [0...SIZE]
      sim.set x, y, null
      jit.grid.set x, y, null

  jit.grid.forEach (x, y, v) ->
    sim.set x, y, null
    jit.grid.set x, y, null

  jit.checkEmpty()

window?.fuzz = fuzz

mainStart = Date.now()
if process.title is 'node'
  fuzz() for [1..10]
console.log "total time", Date.now() - mainStart

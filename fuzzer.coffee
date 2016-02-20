mersenne = require 'mersenne'
assert = require 'assert'
Simulator = require 'boilerplate-sim'
{parseXY} = Simulator
Jit = require './jit'
log = require './log'
util = require './util'

mersenne.seed 2

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

randomShuttle = do ->
  rs = randomValueFrom [null, 5,
    'shuttle', 1
    'thinshuttle', 1
  ]

  (bv) ->
    if bv in ['nothing', 'bridge', 'ribbon', 'ribbonbridge'] then rs() else null

grid = {}
#grid[[x,y]] = 'nothing' for x in [1..2] for y in [1..1]
#grid[[2,2]] = 'positive'

#grid = require './test.json'

sim = new Simulator grid
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

fuzz = ->
  log.quiet = yes
  start = Date.now()
  for iter in [1..10000]
    #log.quiet = false
    debugPrint() unless log.quiet
    #jit.check()
    for [1..2]
      x = mersenne.rand() % SIZE
      y = mersenne.rand() % SIZE
      bv = randomBase()
      sv = randomShuttle bv
      log 'set', x, y, bv, sv

      #sim.set x, y, v
      jit.set x, y, bv, sv

    debugPrint() unless log.quiet

    try
      ###
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
      ###

      #copy = new Jit jit.toJSON()


      jit.check()
      jit.step()
      jit.check()

      # The copy and the original should now match
      #if !justCollidedStates
        #copy.step()
        #assert.deepEqual jit.toJSON(), copy.toJSON()

    catch e
      log.quiet = no
      log "****** CRASH ON ITERATION #{iter} ******"

      ###
      log 'jitPressure', jitPressure
      log 'simPressure', simPressure

      log '---- JIT ----'
      printPressure jitPressure
      log '---- SIM ----'
      printPressure simPressure
      log '-------------'
      ###
      debugPrint()
      log '----'
      #copy.printGrid()
      throw e

    unless iter % 10000
      end = Date.now()
      console.log "#{end-start}ms ----- iter #{iter}", process.memoryUsage?()
      start = end

  for x in [0...SIZE]
    for y in [0...SIZE]
      #sim.set x, y, null
      jit.set x, y, null, null

  jit.baseGrid.forEach (x, y) ->
    #sim.set x, y, null
    jit.set x, y, null, null

  jit.checkEmpty()

window?.fuzz = fuzz
window?.jit = jit

if process.title is 'node'
  mainStart = Date.now()
  fuzz() for [1..10]
  console.log "total time", Date.now() - mainStart

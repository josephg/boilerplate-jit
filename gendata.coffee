# This tool populates gendata/ with a bunch of generated testing data files.

fs = require 'fs'
mersenne = require 'mersenne'
Jit = require './jit'

mersenne.seed 2

randomShuttleCell = ->
  val = (if mersenne.rand() % 2 then 0x40 else 0x80)
  # 3/4 chance to try and connect.
  for i in [0...4]
    val |= (1<<i) if mersenne.rand() % 4 > 0
  return val

randomValue = do ->
  VALS = [
    null, 5
    'nothing', 10
    'positive', 1
    'negative', 1
    'shuttle', 2
    'bridge', 5
  ]

  totalWeight = 0
  totalWeight += VALS[i+1] for _, i in VALS by 2
  ->
    r = mersenne.rand() % totalWeight
    for v,i in VALS by 2
      r -= VALS[i+1]
      break if r < 0

    return if v is 'shuttle' then randomShuttleCell() else v

DIR = 'gendata'
fs.mkdirSync DIR if !fs.existsSync DIR
SIZE = 5

for i in [1..10]
  mersenne.seed i
  filename = "#{DIR}/#{i}.json"
  out = fs.createWriteStream filename

  for [1..100]
    grid = base:{}, shuttles:{}
    for x in [0...SIZE] then for y in [0...SIZE]
      if (v = randomValue())
        if typeof v is 'number'
          grid.base[[x,y]] = 'nothing'
          grid.shuttles[[x,y]] = v
        else
          grid.base[[x,y]] = v

    #require('./util').printJSONGrid grid

    steps = []
    jit = new Jit grid
    #jit.printGrid()
    for [1..5]
      jit.step()
      jit.check()

      steps.push jit.toJSON()
      
    #console.log 'writing'
    out.write "#{JSON.stringify {initial:grid, steps}}\n"

  out.end()
  console.log 'wrote', filename





# This tool populates gendata/ with a bunch of generated testing data files.

fs = require 'fs'
mersenne = require 'mersenne'
Jit = require './jit'

mersenne.seed 2

randomValue = do ->
  VALS = [
    null, 5
    'nothing', 10
    'positive', 1
    'negative', 1
    'shuttle', 1
    #'thinshuttle', 1
    'bridge', 5
  ]

  totalWeight = 0
  totalWeight += VALS[i+1] for _, i in VALS by 2
  ->
    r = mersenne.rand() % totalWeight
    for v,i in VALS by 2
      r -= VALS[i+1]
      break if r < 0

    v

DIR = 'gendata'
fs.mkdirSync DIR if !fs.existsSync DIR
SIZE = 5

for i in [1..10]
  mersenne.seed i
  out = fs.createWriteStream "#{DIR}/#{i}.json"

  for [1..100]
    grid = base:{}, shuttles:{}
    for x in [0...SIZE] then for y in [0...SIZE]
      if (v = randomValue())
        if v in ['shuttle', 'thinshuttle']
          grid.base[[x,y]] = 'nothing'
          grid.shuttles[[x,y]] = v
        else
          grid.base[[x,y]] = v

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





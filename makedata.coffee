# Test data making script. Use it like this:
#
# 1. Make some crazy monstrosity.
# 2. Copy it to the clipboard
# 3. pbpaste | coffee makedata.coffee > foo.json

Jit = require './jit'

isEmpty = (obj) ->
  for k of obj
    return false
  return true

input = ''
process.stdin.on 'data', (data) ->
  input += data.toString 'utf8'

process.stdin.on 'end', ->
  grid = JSON.parse input
  delete grid.tw
  delete grid.th

  #console.log grid
  jit = new Jit grid

  # Simulate 100 steps, or until the simulator loops or stops changing.
  seenState = new Set
  for [1..100]
    key = JSON.stringify jit.toJSON()

    console.log key

    break if seenState.has key
    seenState.add key

    jit.step()

  return

log = require './log'

data = new Map

data.set2 = (k, v) ->
  @set k, v
 
  o k, v for o in @observers

data.del2 = (k) ->
  @delete k
  o k for o in @observers

data.observers = []

data.addObserver = (fn) ->
  @forEach (v, k) ->
    fn k, v
  @observers.push fn

# data -> data2

data2 = new Map
data2.regen = (x) ->
  @delete x
  sum = (data.get(x-1)||'-') + (data.get(x)||'-') + (data.get(x+1)||'-')
  if sum isnt '---'
    @set x, sum

data.addObserver (k, v) ->
  data2.regen k-1
  data2.regen k
  data2.regen k+1



data.set2 1, 'a'
data.set2 2, 'b'
data.set2 3, 'c'

data.del2 3

log data2





#sentSignals = 0
#process.on 'exit', -> console.log 'ss', sentSignals

module.exports = class Watcher
  constructor: (@forEach) ->
    if typeof @forEach isnt 'function'
      container = @forEach
      @forEach = (fn) -> container.forEach fn

    @observers = []

  forward: (fn) ->
    @forEach fn
    @observers.push fn

  on: (fn) ->
    @observers.push fn

  signal: (args...) ->
    #sentSignals++
    o(args...) for o in @observers
    return





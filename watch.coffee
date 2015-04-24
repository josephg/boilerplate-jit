log = require './log'

inTransaction = 0
pendingWatchers = new Set

exports.txn = (fn) ->
  inTransaction++
  fn()
  inTransaction--
  if !inTransaction
    while pendingWatchers.size
      #log 'flushing all'
      pendingWatchers.forEach (watch) ->
        watch.flush()


exports.Watcher = class Watcher
  constructor: (@forEach) ->
    if typeof @forEach isnt 'function'
      container = @forEach
      @forEach = (fn) -> container.forEach fn

    @observers = []
    @heldSignals = []

  on: (fn) ->
    @forEach fn
    @observers.push fn

  signalImm: (args...) ->
    o(args...) for o in @observers
    return

  signal: (args...) ->
    if inTransaction
      @heldSignals.push args
      pendingWatchers.add this
    else
      @signalImm args...

  flush: ->
    pendingWatchers.delete this
    # Danger - if it signals itself, we might not notice.
    @signalImm args... for args in @heldSignals
    @heldSignals.length = 0





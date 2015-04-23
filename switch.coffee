# A switch is a data structure which is an optimised tree / flattened list of
# data keyed by the combined states of shuttles.

class Switch
  constructor: (@sidList) ->
    @key = @sidList.join ','
    # For later :]
    #@flatList = []
    @tree = null

  get: (shuttleStates) -> # This will need a @shuttles argument too to do the flattening optimization
    container = @tree
    for sid in @sidList
      stateid = shuttleStates[sid]
      return null if !container
      container = container[stateid]

    container

  set: (shuttleStates, value) ->
    k = 'tree'
    container = this

    for sid in @sidList
      stateid = shuttleStates[sid]

      container[k] ||= []
      container = container[k]
      k = stateid

    container[k] = value

  innerEach = (list, fn) ->
    if Array.isArray list
      innerEach item, fn for item in list
    else if list
      fn list

  each: (fn) ->
    innerEach @tree, fn

module.exports = Switch


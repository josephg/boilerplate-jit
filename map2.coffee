
module.exports = class Map2 # A map from (a,b) -> c instead of just a->c
  constructor: (data) ->
    @levelOne = new Map
    if data
      for [k1, k2, v] in data
        @set k1, k2, v

  get: (k1, k2) ->
    inner = @levelOne.get k1
    return inner.get k2 if inner

  has: (k1, k2) ->
    inner = @levelOne.get k1
    return inner?.has(k2) or false

  set: (k1, k2, v) ->
    inner = @levelOne.get k1
    if !inner
      inner = new Map
      @levelOne.set k1, inner

    inner.set k2, v

  delete: (k1, k2) ->
    inner = @levelOne.get k1
    inner.delete k2 if inner

  forEach: (fn) ->
    @levelOne.forEach (inner, k1) ->
      inner.forEach (v, k2) ->
        fn k1, k2, v


